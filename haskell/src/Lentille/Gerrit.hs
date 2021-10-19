{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors
--
-- Monocle Gerrit crawler system
module Lentille.Gerrit
  ( getProjectsStream,
    getChangesStream,
    G.GerritProjectQuery (..),
    G.GerritQuery (..),

    -- * The context
    MonadGerrit (..),
    getGerritEnv,
    GerritEnv (..),
    G.getClient,
  )
where

import qualified Data.Attoparsec.Text as P
import Data.Char
import qualified Data.Map as M (keys, lookup, toList)
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector as V
import qualified Gerrit as G
import Gerrit.Data.Change
import Gerrit.Data.Project (GerritProjectsMessage)
import qualified Google.Protobuf.Timestamp as T
import Lentille
-- import Lentille.Env
import Lentille.GitLab.Adapter (diffTime, fromIntToInt32, getChangeId, ghostIdent, toIdent)
import Monocle.Backend.Index (getEventType)
import qualified Monocle.Change as C
import Monocle.Prelude hiding (all, id)
import qualified Monocle.Project as P
import qualified Network.URI as URI
import Proto3.Suite (Enumerated (..))
import qualified Streaming.Prelude as S
import Prelude (last)

-------------------------------------------------------------------------------
-- Gerrit context
-------------------------------------------------------------------------------

class (MonadRetry m, MonadLog m, MonadError LentilleError m) => MonadGerrit m where
  getGerritClient :: Text -> Maybe (Text, Text) -> m G.GerritClient
  getProjects :: GerritEnv -> Int -> G.GerritProjectQuery -> Maybe Int -> m GerritProjectsMessage
  queryChanges :: GerritEnv -> Int -> [GerritQuery] -> Maybe Int -> m [GerritChange]

instance MonadGerrit LentilleM where
  getGerritClient url = liftIO . G.getClient url
  getProjects env count query startM =
    liftIO $ G.getProjects count query startM (client env)
  queryChanges env count queries startM =
    liftIO $ G.queryChanges count queries startM (client env)

data GerritEnv = GerritEnv
  { -- | The Gerrit connexion client
    client :: G.GerritClient,
    -- | A project fullname prefix as defined in the Monocle configuration
    prefix :: Maybe Text,
    -- | The identity alias callback
    identAliasCB :: Maybe (Text -> Maybe Text)
  }

getGerritEnv ::
  -- | The Gerrit connexion client
  G.GerritClient ->
  -- | A project fullname prefix as defined in the Monocle configuration
  Maybe Text ->
  -- | The identity alias callback
  Maybe (Text -> Maybe Text) ->
  GerritEnv
getGerritEnv = GerritEnv

-------------------------------------------------------------------------------
-- Monocle Gerrit crawler entry points for Macroscope
-------------------------------------------------------------------------------

getProjectsStream ::
  MonadGerrit m =>
  GerritEnv ->
  Text ->
  S.Stream (S.Of P.Project) m ()
getProjectsStream env reProject = streamProject env (G.Regexp reProject)

getChangesStream ::
  MonadGerrit m =>
  GerritEnv ->
  UTCTime ->
  Text ->
  S.Stream (S.Of (C.Change, [C.ChangeEvent])) m ()
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

getHostFromURL :: Text -> Text
getHostFromURL url =
  maybe
    (error "Unable to parse provided url")
    (toText . URI.uriRegName)
    (URI.uriAuthority =<< URI.parseURI (toString url))

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

streamProject ::
  MonadGerrit m =>
  GerritEnv ->
  G.GerritProjectQuery ->
  S.Stream (S.Of P.Project) m ()
streamProject env query = go 0
  where
    size = 100
    doGet offset = getProjects env size query (Just offset)
    go offset = do
      projects <- lift $ do retry . doGet $ offset
      let pNames = M.keys projects
      S.each $ P.Project . toLazy <$> pNames
      when (length pNames == size) $ go (offset + size)

streamChange ::
  MonadGerrit m =>
  GerritEnv ->
  [GerritQuery] ->
  S.Stream (S.Of (C.Change, [C.ChangeEvent])) m ()
streamChange env query =
  streamChange' env (G.serverUrl $ client env) query (prefix env) (identAliasCB env)

streamChange' ::
  MonadGerrit m =>
  GerritEnv ->
  Text ->
  [GerritQuery] ->
  Maybe Text ->
  Maybe (Text -> Maybe Text) ->
  S.Stream (S.Of (C.Change, [C.ChangeEvent])) m ()
streamChange' env serverUrl query prefixM identCB = go 0
  where
    size = 100
    go offset = do
      changes <- lift $ do retry . doGet $ offset
      S.each $ (\c -> let cT = toMChange c in (cT, toMEvents cT (messages c))) <$> changes
      when (length changes == size) $ go (offset + size)
    doGet offset = queryChanges env size query (Just offset)
    prefix = fromMaybe "" prefixM
    getIdent :: GerritAuthor -> C.Ident
    getIdent gAuthor = toIdent (getHostFromURL serverUrl) identCB $ toAuthorName (aName gAuthor) (show . aAccountId $ gAuthor)
      where
        toAuthorName name accountID = name <> "/" <> accountID
    toMEvents :: C.Change -> [GerritChangeMessage] -> [C.ChangeEvent]
    toMEvents C.Change {..} messages =
      [toChangeCreatedEvent]
        <> toChangeMergedEvent
        <> toChangeAbandonedEvent
        <> toChangeReviewedEvents
        <> toChangeCommentedEvents
        <> toChangePushedEvents
      where
        baseEvent :: C.ChangeEventType -> C.ChangeEvent
        baseEvent eType =
          let changeEventId = eventTypeToText eType <> changeId
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
              changeEventChangedFiles = C.ChangedFilePath . C.changedFilePath <$> changeChangedFiles
              changeEventLabels = changeLabels
              changeEventType = Just eType
           in C.ChangeEvent {..}
          where
            eventTypeToText t = into @LText . getEventType $ Just t
        toChangeCreatedEvent = baseEvent $ C.ChangeEventTypeChangeCreated C.ChangeCreatedEvent
        toChangeMergedEvent = case changeState of
          Enumerated (Right C.Change_ChangeStateMerged) ->
            [ (baseEvent $ C.ChangeEventTypeChangeMerged C.ChangeMergedEvent)
                { C.changeEventAuthor = case changeOptionalMergedBy of
                    Just (C.ChangeOptionalMergedByMergedBy ident) -> Just ident
                    Nothing -> Nothing,
                  C.changeEventCreatedAt = case changeOptionalMergedAt of
                    Just (C.ChangeOptionalMergedAtMergedAt ts) -> Just ts
                    Nothing -> Nothing
                }
            ]
          _ -> mempty
        toChangeAbandonedEvent = case changeState of
          Enumerated (Right C.Change_ChangeStateClosed) ->
            [ (baseEvent $ C.ChangeEventTypeChangeAbandoned C.ChangeAbandonedEvent)
                { C.changeEventCreatedAt = case changeOptionalClosedAt of
                    Just (C.ChangeOptionalClosedAtClosedAt ts) -> Just ts
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
                    (C.ChangeEventTypeChangeReviewed . C.ChangeReviewedEvent $ V.fromList $ toLazy <$> approvals)
                    mAuthor
                    mDate
              Left _ -> Nothing
        toChangeCommentedEvents =
          toCommentBasedEvent commentParser (C.ChangeEventTypeChangeCommented C.ChangeCommentedEvent)
        toChangePushedEvents =
          toCommentBasedEvent newPSParser (C.ChangeEventTypeChangeCommitPushed C.ChangeCommitPushedEvent)
        toCommentBasedEvent parser eType = mapMaybe toEvent messages
          where
            toEvent GerritChangeMessage {..} = case P.parseOnly parser mMessage of
              Right _ ->
                Just $ commentBasedEvent eType mAuthor mDate
              Left _ -> Nothing
        commentBasedEvent t author date =
          (baseEvent t)
            { C.changeEventAuthor = getIdent <$> author,
              C.changeEventCreatedAt = Just . T.fromUTCTime . unGerritTime $ date
            }

    toMChange :: GerritChange -> C.Change
    toMChange GerritChange {..} =
      let changeId = toLazy id
          changeNumber = fromIntToInt32 number
          changeChangeId = getChangeId project (show number)
          changeTitle = toLazy subject
          changeText = getCommitMessage
          changeUrl = toLazy $ (T.dropWhileEnd (== '/') serverUrl) <> "/" <> show changeNumber
          changeCommitCount = 1
          changeAdditions = fromIntToInt32 insertions
          changeDeletions = fromIntToInt32 deletions
          changeChangedFilesCount = getFilesCount
          changeChangedFiles = V.fromList getFiles
          changeCommits = V.fromList $ maybe [] getCommits current_revision
          changeRepositoryPrefix = toLazy $ prefix <> T.intercalate "/" (reverse (drop 1 $ reverse $ T.split (== '/') project))
          changeRepositoryFullname =
            if T.null $ toText changeRepositoryPrefix
              then changeRepositoryShortname
              else changeRepositoryPrefix <> "/" <> changeRepositoryShortname
          changeRepositoryShortname = toLazy . Prelude.last $ T.split (== '/') project
          changeAuthor = Just author
          changeOptionalMergedBy =
            if status == MERGED
              then C.ChangeOptionalMergedByMergedBy <$> merger
              else Nothing
          changeBranch = toLazy branch
          changeTargetBranch = toLazy branch
          changeCreatedAt = Just $ toTimestamp created
          changeOptionalMergedAt =
            if status == MERGED
              then Just . C.ChangeOptionalMergedAtMergedAt $ maybe (toTimestamp updated) toTimestamp submitted
              else Nothing
          changeUpdatedAt = Just $ toTimestamp updated
          changeOptionalClosedAt = case status of
            ABANDONED -> Just $ C.ChangeOptionalClosedAtClosedAt $ toTimestamp updated
            MERGED -> Just $ C.ChangeOptionalClosedAtClosedAt $ maybe (toTimestamp updated) toTimestamp submitted
            _ -> Nothing
          changeState = toState status
          changeOptionalDuration =
            C.ChangeOptionalDurationDuration . fromIntToInt32 . diffTime (unGerritTime created)
              <$> (unGerritTime <$> submitted)
          changeMergeable = case mergeable of
            Just True -> "MERGEABLE"
            _ -> "CONFLICT"
          changeLabels = V.fromList $ toLazy <$> hashtags <> maybe mempty (: []) topic
          -- TODO(fbo) add assignees support to gerrit-haskell
          changeAssignees = V.fromList []
          changeApprovals = V.fromList $ toLazy <$> toApprovals (M.toList labels)
          changeDraft = status == DRAFT
          changeOptionalSelfMerged =
            if status == MERGED
              then C.ChangeOptionalSelfMergedSelfMerged . isSelfMerged owner <$> submitter
              else Nothing
       in C.Change {..}
      where
        getRevision sha = join (M.lookup sha revisions)
        revision = getRevision =<< current_revision
        author = getIdent owner
        uploader = grUploader <$> revision
        merger = getIdent <$> submitter
        isSelfMerged s a = aAccountId s == aAccountId a
        toTimestamp = T.fromUTCTime . unGerritTime
        ghostIdent' = ghostIdent $ getHostFromURL serverUrl
        getCommitMessage = maybe "" (toLazy . cMessage . grCommit) revision
        getFilesCount = fromIntToInt32 $ maybe 0 (length . M.keys . grFiles) revision
        getFiles = maybe [] (fmap toChangeFile) $ M.toList . grFiles <$> revision
          where
            toChangeFile (fp, details) =
              let changedFileAdditions = maybe 0 fromIntToInt32 $ gfLinesInserted details
                  changedFileDeletions = maybe 0 fromIntToInt32 $ gfLinesDeleted details
                  changedFilePath = toLazy fp
               in C.ChangedFile {..}
        getCommits sha = maybe [] toCommit $ grCommit <$> revision
          where
            toCommit GerritCommit {..} =
              let commitSha = toLazy sha
                  commitAuthor = Just author
                  commitCommitter = Just $ maybe ghostIdent' getIdent uploader
                  commitAuthoredAt = Just . T.fromUTCTime . unGerritTime . caDate $ cAuthor
                  commitCommittedAt = Just . T.fromUTCTime . unGerritTime . caDate $ cCommitter
                  commitAdditions = fromIntToInt32 insertions
                  commitDeletions = fromIntToInt32 deletions
                  commitTitle = toLazy cSubject
               in [C.Commit {..}]
        toState :: GerritChangeStatus -> Enumerated C.Change_ChangeState
        toState status' = Enumerated . Right $ case status' of
          ABANDONED -> C.Change_ChangeStateClosed
          MERGED -> C.Change_ChangeStateMerged
          NEW -> C.Change_ChangeStateOpen
          DRAFT -> C.Change_ChangeStateOpen
