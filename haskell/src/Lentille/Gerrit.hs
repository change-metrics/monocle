module Lentille.Gerrit
  ( streamProject,
    streamChange,
    GerritProjectQuery (..),
    GerritQuery (..),
  )
where

import Control.Monad.IO.Unlift
import qualified Data.Map as M (elems, keys, lookup, toList)
import qualified Data.Text as T
import qualified Data.Vector as V
import Gerrit hiding (changeUrl)
import qualified Gerrit as G (changeUrl)
import Gerrit.Data.Change
import Gerrit.Data.Project (GerritProjectInfo (gerritprojectinfoId))
import qualified Google.Protobuf.Timestamp as T
import Lentille (LentilleStream)
import Lentille.GitLab.Adapter (diffTime, fromIntToInt32, getChangeId, ghostIdent, toIdent)
import qualified Monocle.Change as C
import Monocle.Client (retry)
import qualified Monocle.Project as P
import qualified Network.URI as URI
import Proto3.Suite (Enumerated (..))
import Relude hiding (all, id)
import qualified Streaming.Prelude as S
import Prelude (last)

-- TODO(fbo): grabbed from Lentille.Gitlab - migrate to a Lentille.Common
getHostFromURL :: Text -> Text
getHostFromURL url =
  maybe
    (error "Unable to parse provided gitlab_url")
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

streamProject :: GerritClient -> GerritProjectQuery -> LentilleStream P.Project
streamProject client query = go 0
  where
    size = 100
    go offset = do
      projects <- liftIO $ retry $ getProjects size query (Just offset) client
      let pNames = gerritprojectinfoId <$> M.elems projects
      S.each $ P.Project . toLazy <$> pNames
      when (length pNames == size) $ go (offset + size)

streamChange :: GerritClient -> [GerritQuery] -> Maybe Text -> LentilleStream C.Change
streamChange client query prefixM = go 0
  where
    size = 100
    go offset = do
      changes <- liftIO $ retry $ queryChanges size query (Just offset) client
      S.each $ toMChange <$> changes
      when (length changes == size) $ go (offset + size)
    getHost = getHostFromURL $ serverUrl client
    prefix = fromMaybe "" prefixM
    toAuthorName name accountID = name <> "/" <> accountID
    toMChange :: GerritChange -> C.Change
    toMChange GerritChange {..} =
      let changeId = toLazy id
          changeNumber = fromIntToInt32 number
          changeChangeId = getChangeId project (show number)
          changeTitle = toLazy subject
          changeText = getCommitMessage
          changeUrl = toLazy $ G.changeUrl client GerritChange {..}
          changeCommitCount = 1
          changeAdditions = fromIntToInt32 insertions
          changeDeletions = fromIntToInt32 deletions
          changeChangedFilesCount = getFilesCount
          changeChangedFiles = V.fromList getFiles
          changeCommits = V.fromList $ maybe [] getCommits current_revision
          changeRepositoryPrefix = toLazy $ prefix <> T.intercalate "/" (reverse (drop 1 $ reverse $ T.split (== '/') project))
          changeRepositoryFullname = changeRepositoryPrefix <> "/" <> changeRepositoryShortname
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
          -- Gerrit labels must be handled as Review
          changeLabels = V.fromList []
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
        revision = join $ getRevision <$> current_revision
        author = toIdent getHost Nothing $ toAuthorName (aName owner) (show . aAccountId $ owner)
        uploader = grUploader <$> revision
        merger = (\s -> toIdent getHost Nothing $ toAuthorName (aName s) (show . aAccountId $ s)) <$> submitter
        isSelfMerged s a = aAccountId s == aAccountId a
        toTimestamp = T.fromUTCTime . unGerritTime
        ghostIdent' = ghostIdent getHost
        getCommitMessage = fromMaybe "" $ toLazy . cMessage . grCommit <$> revision
        getFilesCount = fromIntToInt32 $ fromMaybe 0 $ length . M.keys . grFiles <$> revision
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
              -- TODO(fbo) ensure alias callback is passed
              let commitSha = toLazy sha
                  commitAuthor = Just author
                  commitCommitter =
                    Just $
                      maybe
                        ghostIdent'
                        (\ga -> toIdent getHost Nothing $ toAuthorName (aName ga) (show . aAccountId $ ga))
                        uploader
                  commitAuthoredAt = Just . T.fromUTCTime . unGerritTime . caDate $ cAuthor
                  commitCommittedAt = Just . T.fromUTCTime . unGerritTime . caDate $ cCommitter
                  commitAdditions = fromIntToInt32 insertions
                  commitDeletions = fromIntToInt32 deletions
                  commitTitle = toLazy cSubject
               in [C.Commit {..}]
        toState :: GerritChangeStatus -> Enumerated C.Change_ChangeState
        toState = \case
          ABANDONED -> Enumerated (Right C.Change_ChangeStateClosed)
          MERGED -> Enumerated $ Right C.Change_ChangeStateMerged
          NEW -> Enumerated $ Right C.Change_ChangeStateOpen
          DRAFT -> Enumerated $ Right C.Change_ChangeStateOpen
