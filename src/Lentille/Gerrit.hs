-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors
--
-- Monocle Gerrit crawler system
module Lentille.Gerrit (
  getProjectsStream,
  getChangesStream,
  G.GerritProjectQuery (..),
  G.GerritQuery (..),

  -- * The context
  MonadGerrit (..),
  GerritEnv (..),
  G.getClient,

  -- * Helpers
  streamChange,
  streamProject,
) where

import Data.Attoparsec.Text qualified as P
import Data.Char
import Data.Map qualified as M (keys, lookup, toList)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Vector qualified as V
import Gerrit qualified as G
import Gerrit.Data.Change
import Gerrit.Data.Project (GerritProjectsMessage)
import Google.Protobuf.Timestamp qualified as T
import Lentille
import Monocle.Client (mkManager)
import Monocle.Prelude hiding (all, id)
import Monocle.Protob.Change qualified as ChangePB
import Monocle.Protob.Crawler qualified as CrawlerPB
import Network.URI qualified as URI
import Proto3.Suite (Enumerated (..))
import Streaming.Prelude qualified as S
import Prelude (init, last)

-------------------------------------------------------------------------------
-- Gerrit context
-------------------------------------------------------------------------------

class (MonadRetry m, MonadMonitor m) => MonadGerrit m where
  getGerritClient :: Text -> Maybe (Text, Secret) -> m G.GerritClient
  getProjects :: GerritEnv -> Int -> G.GerritProjectQuery -> Maybe Int -> m GerritProjectsMessage
  queryChanges :: GerritEnv -> Int -> [GerritQuery] -> Maybe Int -> m [GerritChange]

instance MonadGerrit LoggerT where
  getGerritClient url = liftIO . getGerritClient url
  getProjects env count query = liftIO . getProjects env count query
  queryChanges env count queries = liftIO . queryChanges env count queries

instance MonadGerrit LentilleM where
  getGerritClient url = liftIO . getGerritClient url
  getProjects env count query = liftIO . getProjects env count query
  queryChanges env count queries = liftIO . queryChanges env count queries

instance MonadGerrit IO where
  getGerritClient url Nothing = getClient url Nothing
  getGerritClient url (Just (user, Secret password)) = getClient url $ Just (user, password)
  getProjects env count query startM = G.getProjects count query startM (client env)
  queryChanges env count queries startM = G.queryChanges count queries startM (client env)

getClient :: Text -> Maybe (Text, Text) -> IO G.GerritClient
getClient url auth = do
  manager <- mkManager
  pure $ G.getClientWithManager manager url auth

data GerritEnv = GerritEnv
  { client :: G.GerritClient
  -- ^ The Gerrit connexion client
  , prefix :: Maybe Text
  -- ^ A project fullname prefix as defined in the Monocle configuration
  , identAliasCB :: Text -> Maybe Text
  -- ^ The identity alias callback
  , crawlerName :: Text
  -- ^ The crawler name
  }

-------------------------------------------------------------------------------
-- Monocle Gerrit crawler entry points for Macroscope
-------------------------------------------------------------------------------

getProjectsStream ::
  (HasLogger m, MonadGerrit m) =>
  GerritEnv ->
  Text ->
  S.Stream (S.Of CrawlerPB.Project) m ()
getProjectsStream env reProject = streamProject env (G.Regexp reProject)

getChangesStream ::
  (HasLogger m, MonadGerrit m) =>
  GerritEnv ->
  UTCTime ->
  Text ->
  S.Stream (S.Of (ChangePB.Change, [ChangePB.ChangeEvent])) m ()
getChangesStream env untilDate project = streamChange env [Project project, After untilDate]

-------------------------------------------------------------------------------
-- Monocle Gerrit crawler system
-------------------------------------------------------------------------------

-- >>> P.parseOnly approvalsParser "Patch Set 10: Code-Review+2 Workflow+1"
-- Right ["Code-Review+2","Workflow+1"]
-- >>> P.parseOnly approvalsParser "Patch Set 1:\n\nStarting check jobs."
-- Left "string"
-- >>> P.parseOnly approvalsParser "Patch Set 3: Verified-1\n\nBuild failed."
-- Right ["Verified-1"]
approvalsParser :: P.Parser [Text]
approvalsParser = do
  void $ P.string "Patch Set "
  void (P.decimal :: P.Parser Integer)
  void $ P.string ": "
  word `P.sepBy1` P.char ' '
 where
  word = P.takeWhile1 (not . isSpace)

-- >>> P.parseOnly commentParser "Patch Set 3: Verified-1\n\nBuild failed."
-- Right "Build failed."
-- >>> P.parseOnly commentParser "Patch Set 3: Verified-1"
-- Left "'\\n': not enough input"
commentParser :: P.Parser Text
commentParser = do
  void $ P.takeTill (== '\n')
  void $ P.count 2 $ P.char '\n'
  P.takeText

newPSParser :: P.Parser Text
newPSParser = P.string "Uploaded patch set"

-- >>> getHostFromURL "https://softwarefactory-project.io/r/22969"
-- "softwarefactory-project.io"
getHostFromURL :: Text -> Text
getHostFromURL url =
  maybe
    (error "Unable to parse provided url")
    (from . URI.uriRegName)
    (URI.uriAuthority =<< URI.parseURI (from url))

{-
>>> let v1 = GerritDetailedLabelVote {value = Just 2, account_id = 42}
>>> let v2 = GerritDetailedLabelVote {value = Just 0, account_id = 42}
>>> let v3 = GerritDetailedLabelVote {value = Just (-1), account_id = 42}
>>> let app1 = ("Code-Review", GerritDetailedLabel {all = Just [v1, v2, v3], default_value = 0})
>>> let app2 = ("Verified", GerritDetailedLabel {all = Just [v1, v2, v3], default_value = 0})
>>> toApprovals [app1, app2]
["Code-Review+2","Code-Review-1","Verified+2","Verified-1"]
-}
toApprovals :: [(Text, GerritDetailedLabel)] -> [Text]
toApprovals = concatMap genApprovals
 where
  genApprovals :: (Text, GerritDetailedLabel) -> [Text]
  genApprovals (label, GerritDetailedLabel {..}) =
    (label <>) <$> case all of
      Just votes -> filter (/= "+0") $ genTextVal <$> votes
      Nothing -> []
  genTextVal :: GerritDetailedLabelVote -> Text
  genTextVal vote = case value vote of
    Just v -> if v >= 0 then "+" <> show v else show v
    Nothing -> "+0"

-- >>> getPrefix Nothing "config"
-- ""
-- >>> getPrefix (Just "org/") "config"
-- "org"
-- >>> getPrefix Nothing "rpms/nova-distgit"
-- "rpms"
-- >>> getPrefix (Just "org/") "rpms/nova-distgit"
-- "org/rpms"
getPrefix :: Maybe Text -> Text -> LText
getPrefix prefixM project =
  let parts = T.split (== '/') project
      parts' = Prelude.init parts
      prefix = fromMaybe mempty prefixM
   in from $ T.dropWhileEnd (== '/') $ prefix <> T.intercalate "/" parts'

streamProject ::
  (HasLogger m, MonadGerrit m) =>
  GerritEnv ->
  G.GerritProjectQuery ->
  S.Stream (S.Of CrawlerPB.Project) m ()
streamProject env query = go 0
 where
  size = 100
  doGet offset = getProjects env size query (Just offset)
  go offset = do
    projects <- lift $ do httpRetry (crawlerName env, G.serverUrl $ client env, "crawler") . doGet $ offset
    let pNames = M.keys projects
    S.each $ CrawlerPB.Project . from <$> pNames
    when (length pNames == size) $ go (offset + size)

streamChange ::
  (HasLogger m, MonadGerrit m) =>
  GerritEnv ->
  [GerritQuery] ->
  S.Stream (S.Of (ChangePB.Change, [ChangePB.ChangeEvent])) m ()
streamChange env query =
  streamChange' env (identAliasCB env) (G.serverUrl $ client env) query (prefix env)

streamChange' ::
  (HasLogger m, MonadGerrit m) =>
  GerritEnv ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  Text ->
  [GerritQuery] ->
  Maybe Text ->
  S.Stream (S.Of (ChangePB.Change, [ChangePB.ChangeEvent])) m ()
streamChange' env identCB serverUrl query prefixM = go 0
 where
  size = 100
  go offset = do
    changes <- lift $ do httpRetry (crawlerName env, G.serverUrl $ client env, "crawler") . doGet $ offset
    S.each $ (\c -> let cT = toMChange c in (cT, toMEvents cT (messages c))) <$> changes
    when (length changes == size) $ go (offset + size)
  doGet offset = queryChanges env size query (Just offset)
  getIdent :: GerritAuthor -> ChangePB.Ident
  getIdent GerritAuthor {..} =
    toIdent
      (getHostFromURL serverUrl)
      identCB
      $ name <> "/" <> show aAccountId
   where
    name = fromMaybe nobody aName
  toMEvents :: ChangePB.Change -> [GerritChangeMessage] -> [ChangePB.ChangeEvent]
  toMEvents ChangePB.Change {..} messages =
    [toChangeCreatedEvent]
      <> toChangeMergedEvent
      <> toChangeAbandonedEvent
      <> toChangeReviewedEvents
      <> toChangeCommentedEvents
      <> toChangePushedEvents
   where
    baseEvent :: ChangePB.ChangeEventType -> LText -> ChangePB.ChangeEvent
    baseEvent eType eId =
      let changeEventId = eId
          changeEventCreatedAt = changeCreatedAt
          changeEventAuthor = changeAuthor
          changeEventRepositoryFullname = changeRepositoryFullname
          changeEventRepositoryPrefix = changeRepositoryPrefix
          changeEventRepositoryShortname = changeRepositoryShortname
          changeEventBranch = changeBranch
          changeEventTargetBranch = changeTargetBranch
          changeEventNumber = changeNumber
          changeEventChangeId = changeChangeId
          changeEventUrl = changeUrl
          changeEventOnAuthor = changeAuthor
          changeEventOnCreatedAt = changeCreatedAt
          changeEventChangedFiles = ChangePB.ChangedFilePath . ChangePB.changedFilePath <$> changeChangedFiles
          changeEventLabels = changeLabels
          changeEventType = Just eType
          changeEventOptionalDuration = swapDuration <$> changeOptionalDuration
       in ChangePB.ChangeEvent {..}
    toChangeCreatedEvent =
      baseEvent (ChangePB.ChangeEventTypeChangeCreated ChangePB.ChangeCreatedEvent) $ "CCE" <> changeId

    toChangeMergedEvent = case changeState of
      Enumerated (Right ChangePB.Change_ChangeStateMerged) ->
        [ (baseEvent (ChangePB.ChangeEventTypeChangeMerged ChangePB.ChangeMergedEvent) $ "CCLE" <> changeId)
            { ChangePB.changeEventAuthor = case changeOptionalMergedBy of
                Just (ChangePB.ChangeOptionalMergedByMergedBy ident) -> Just ident
                Nothing -> Nothing
            , ChangePB.changeEventCreatedAt = case changeOptionalMergedAt of
                Just (ChangePB.ChangeOptionalMergedAtMergedAt ts) -> Just ts
                Nothing -> Nothing
            }
        ]
      _ -> mempty
    toChangeAbandonedEvent = case changeState of
      Enumerated (Right ChangePB.Change_ChangeStateClosed) ->
        [ (baseEvent (ChangePB.ChangeEventTypeChangeAbandoned ChangePB.ChangeAbandonedEvent) $ "CCLE" <> changeId)
            { ChangePB.changeEventCreatedAt = case changeOptionalClosedAt of
                Just (ChangePB.ChangeOptionalClosedAtClosedAt ts) -> Just ts
                Nothing -> Nothing
            }
        ]
      _ -> mempty
    toChangeReviewedEvents = mapMaybe toReviewEvent messages
     where
      toReviewEvent GerritChangeMessage {..} = case P.parseOnly approvalsParser mMessage of
        Right approvals ->
          Just $
            commentBasedEvent
              (ChangePB.ChangeEventTypeChangeReviewed . ChangePB.ChangeReviewedEvent $ V.fromList $ from <$> approvals)
              ("approval_" <> from mId)
              mAuthor
              mDate
        Left _ -> Nothing
    toChangeCommentedEvents =
      toCommentBasedEvent commentParser (ChangePB.ChangeEventTypeChangeCommented ChangePB.ChangeCommentedEvent) ""
    toChangePushedEvents =
      toCommentBasedEvent newPSParser (ChangePB.ChangeEventTypeChangeCommitPushed ChangePB.ChangeCommitPushedEvent) "push_"
    toCommentBasedEvent parser eType prefix = mapMaybe toEvent messages
     where
      toEvent GerritChangeMessage {..} = case P.parseOnly parser mMessage of
        Right _ ->
          Just $ commentBasedEvent eType (prefix <> from mId) mAuthor mDate
        Left _ -> Nothing
    commentBasedEvent t eId author date =
      (baseEvent t eId)
        { ChangePB.changeEventAuthor = getIdent <$> author
        , ChangePB.changeEventCreatedAt = Just . T.fromUTCTime . unGerritTime $ date
        }

  toMChange :: GerritChange -> ChangePB.Change
  toMChange GerritChange {..} =
    let changeId = from id
        changeNumber = from number
        changeChangeId = getChangeId project (show number)
        changeTitle = from subject
        changeText = getCommitMessage
        changeUrl = from $ T.dropWhileEnd (== '/') serverUrl <> "/" <> show changeNumber
        changeCommitCount = 1
        changeAdditions = from insertions
        changeDeletions = from deletions
        changeChangedFilesCount = getFilesCount
        changeChangedFiles = V.fromList getFiles
        changeCommits = V.fromList $ maybe [] getCommits current_revision
        changeRepositoryPrefix = getPrefix prefixM project
        changeRepositoryFullname =
          if T.null $ from changeRepositoryPrefix
            then changeRepositoryShortname
            else changeRepositoryPrefix <> "/" <> changeRepositoryShortname
        changeRepositoryShortname = from . Prelude.last $ T.split (== '/') project
        changeAuthor = Just author
        changeOptionalMergedBy =
          if status == MERGED
            then ChangePB.ChangeOptionalMergedByMergedBy <$> merger
            else Nothing
        changeBranch = from branch
        changeTargetBranch = from branch
        changeCreatedAt = Just $ toTimestamp created
        changeOptionalMergedAt =
          if status == MERGED
            then Just . ChangePB.ChangeOptionalMergedAtMergedAt $ maybe (toTimestamp updated) toTimestamp submitted
            else Nothing
        changeUpdatedAt = Just $ toTimestamp updated
        changeOptionalClosedAt = case status of
          ABANDONED -> Just $ ChangePB.ChangeOptionalClosedAtClosedAt $ toTimestamp updated
          MERGED -> Just $ ChangePB.ChangeOptionalClosedAtClosedAt $ maybe (toTimestamp updated) toTimestamp submitted
          _ -> Nothing
        changeState = toState status
        changeOptionalDuration = case submitted of
          Just merged_ts -> Just . ChangePB.ChangeOptionalDurationDuration . from $ diffTimeSec (unGerritTime merged_ts) (unGerritTime created)
          Nothing -> Nothing
        changeMergeable = case mergeable of
          Just False -> "CONFLICT"
          _ -> "MERGEABLE"
        changeLabels = V.fromList $ from <$> hashtags <> maybe mempty (: []) topic
        -- TODO(fbo) add assignees support to gerrit-haskell
        changeAssignees = V.fromList []
        changeApprovals = V.fromList $ from <$> toApprovals (M.toList labels)
        changeDraft = status == DRAFT
        changeOptionalSelfMerged =
          if status == MERGED
            then ChangePB.ChangeOptionalSelfMergedSelfMerged . isSelfMerged owner <$> submitter
            else Nothing
     in ChangePB.Change {..}
   where
    getRevision sha = join (M.lookup sha revisions)
    revision = getRevision =<< current_revision
    author = getIdent owner
    uploader = grUploader <$> revision
    merger = getIdent <$> submitter
    isSelfMerged s a = aAccountId s == aAccountId a
    toTimestamp = T.fromUTCTime . unGerritTime
    ghostIdent' = ghostIdent $ getHostFromURL serverUrl
    getCommitMessage = maybe "" (from . cMessage . grCommit) revision
    getFilesCount = from $ maybe 0 (length . M.keys . grFiles) revision
    getFiles = maybe [] (fmap toChangeFile) $ M.toList . grFiles <$> revision
     where
      toChangeFile (fp, details) =
        let changedFileAdditions = maybe 0 from $ gfLinesInserted details
            changedFileDeletions = maybe 0 from $ gfLinesDeleted details
            changedFilePath = from fp
         in ChangePB.ChangedFile {..}
    getCommits sha = maybe [] toCommit $ grCommit <$> revision
     where
      toCommit GerritCommit {..} =
        let commitSha = from sha
            commitAuthor = Just author
            commitCommitter = Just $ maybe ghostIdent' getIdent uploader
            commitAuthoredAt = Just . T.fromUTCTime . unGerritTime . caDate $ cAuthor
            commitCommittedAt = Just . T.fromUTCTime . unGerritTime . caDate $ cCommitter
            commitAdditions = from insertions
            commitDeletions = from deletions
            commitTitle = from cSubject
         in [ChangePB.Commit {..}]
    toState :: GerritChangeStatus -> Enumerated ChangePB.Change_ChangeState
    toState status' = Enumerated . Right $ case status' of
      ABANDONED -> ChangePB.Change_ChangeStateClosed
      MERGED -> ChangePB.Change_ChangeStateMerged
      NEW -> ChangePB.Change_ChangeStateOpen
      DRAFT -> ChangePB.Change_ChangeStateOpen
