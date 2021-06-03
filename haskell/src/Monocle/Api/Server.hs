{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.Server where

import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Api.Env
import Monocle.Search (ChangesQueryRequest, ChangesQueryResponse, FieldsRequest, FieldsResponse (..))
import qualified Monocle.Search as SearchPB
import Monocle.Search.Change (Author (..), ELKChange (..), TaskData (..))
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import qualified Monocle.TaskData as TaskDataPB
import Proto3.Suite (Enumerated (..))
import Relude
import Servant (Handler)

searchChangeQuery :: ChangesQueryRequest -> AppM ChangesQueryResponse
searchChangeQuery request = do
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
      let changeTitle = (toLazy $ elkchangeTitle change)
          changeUrl = (toLazy $ elkchangeUrl change)
          changeCreatedAt = (Just . Timestamp.fromUTCTime $ elkchangeCreatedAt change)
          changeRepositoryFullname = (toLazy $ elkchangeRepositoryFullname change)
          changeState = (toLazy $ elkchangeState change)
          changeBranch = (toLazy $ elkchangeBranch change)
          changeTaskData = V.fromList . map toTaskData . fromMaybe [] $ elkchangeTasksData change
          changeChangeId = (toLazy $ elkchangeChangeId change)
          changeAuthor = toLazy . authorMuid . elkchangeAuthor $ change
       in SearchPB.Change {..}
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
