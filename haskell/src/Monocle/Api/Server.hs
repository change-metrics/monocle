{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.Server where

import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Backend.Documents (Author (..), Commit (..), ELKChange (..), File (..), TaskData (..))
import Monocle.Search (ChangesQueryRequest, ChangesQueryResponse, FieldsRequest, FieldsResponse (..))
import qualified Monocle.Search as SearchPB
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import Monocle.Servant.Env
import qualified Monocle.TaskData as TaskDataPB
import Proto3.Suite (Enumerated (..))
import Relude

searchChangesQuery :: ChangesQueryRequest -> AppM ChangesQueryResponse
searchChangesQuery request = do
  Env {bhEnv = bhEnv} <- ask
  now <- liftIO getCurrentTime
  SearchPB.ChangesQueryResponse . Just <$> response bhEnv now
  where
    queryText = toStrict $ SearchPB.changesQueryRequestQuery request
    index = "monocle.changes.1." <> toStrict (SearchPB.changesQueryRequestIndex request)
    response bhEnv now = case P.parse queryText >>= Q.queryWithMods now of
      Left (ParseError msg offset) ->
        pure
          . SearchPB.ChangesQueryResponseResultError
          . SearchPB.QueryError (toLazy msg)
          $ (fromInteger . toInteger $ offset)
      Right query ->
        SearchPB.ChangesQueryResponseResultItems
          . SearchPB.Changes
          . V.fromList
          . map toResult
          <$> Q.changes bhEnv index query
    toResult :: ELKChange -> SearchPB.Change
    toResult change =
      let getText field = toLazy (field change)
          changeTitle = getText elkchangeTitle
          changeUrl = getText elkchangeUrl
          changeCreatedAt = (Just . Timestamp.fromUTCTime $ elkchangeCreatedAt change)
          changeUpdatedAt = (Just . Timestamp.fromUTCTime $ elkchangeUpdatedAt change)
          changeRepositoryFullname = getText elkchangeRepositoryFullname
          changeState = getText elkchangeState
          changeBranch = getText elkchangeBranch
          changeTargetBranch = getText elkchangeTargetBranch
          changeTaskData = V.fromList . maybe [] (map toTaskData) $ elkchangeTasksData change
          changeChangeId = getText elkchangeChangeId
          changeAuthor = authorMuid . elkchangeAuthor $ change
          changeText = getText elkchangeText
          changeAdditions = elkchangeAdditions change
          changeDeletions = elkchangeDeletions change
          changeChangedFilesCount = elkchangeChangedFilesCount change
          changeApproval = V.fromList (maybe [] (fmap toLazy) (elkchangeApproval change))
          changeAssignees = V.fromList (fmap authorMuid (elkchangeAssignees change))
          changeLabels = V.fromList (toLazy <$> elkchangeLabels change)
          changeDraft = elkchangeDraft change
          changeMergeable = elkchangeMergeable change == "MERGEABLE"
          changeCommits = V.fromList . map toCommit $ elkchangeCommits change
          changeChangedFiles = V.fromList . map toFile $ elkchangeChangedFiles change
          -- consistency rename from commit_count to commits_count
          changeCommitsCount = elkchangeCommitCount change
          changeMergedAt = toTS =<< elkchangeMergedAt change
          changeMergedByM = Just . SearchPB.ChangeMergedByMMergedBy . authorMuid =<< elkchangeMergedBy change
       in SearchPB.Change {..}
    toTS = Just . Timestamp.fromUTCTime
    toFile File {..} = SearchPB.File {..}
    toCommit :: Commit -> SearchPB.Commit
    toCommit Commit {..} =
      let commitSha = elkcommitSha
          commitTitle = elkcommitTitle
          commitAuthor = authorMuid elkcommitAuthor
          commitAuthoredAt = toTS elkcommitAuthoredAt
          commitCommitter = authorMuid elkcommitCommitter
          commitCommittedAt = toTS elkcommitCommittedAt
          commitAdditions = elkcommitAdditions
          commitDeletions = elkcommitDeletions
       in SearchPB.Commit {..}
    toTaskData :: TaskData -> TaskDataPB.NewTaskData
    toTaskData td =
      let newTaskDataUpdatedAt = Nothing
          newTaskDataChangeUrl = toLazy $ tdUrl td
          newTaskDataTtype = fmap toLazy $ V.fromList $ tdTtype td
          newTaskDataTid = toLazy $ tdTid td
          newTaskDataUrl = toLazy $ tdUrl td
          newTaskDataTitle = toLazy $ tdUrl td
          newTaskDataSeverity = toLazy $ tdSeverity td
          newTaskDataPriority = toLazy $ tdPriority td
          newTaskDataScore = fromInteger $ toInteger $ tdScore td
       in TaskDataPB.NewTaskData {..}

searchFields :: FieldsRequest -> AppM FieldsResponse
searchFields = const $ pure response
  where
    response :: FieldsResponse
    response = FieldsResponse . V.fromList . map toResult $ Q.fields
    toResult (name, (fieldType', _realname, desc)) =
      let fieldName = toLazy name
          fieldDescription = toLazy desc
          fieldType = Enumerated . Right $ fieldType'
       in SearchPB.Field {..}
