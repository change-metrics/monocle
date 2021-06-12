{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.Server where

import Data.Fixed (Deci)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as Timestamp
import qualified Monocle.Auth as AuthPB
import Monocle.Backend.Documents (Author (..), Commit (..), ELKChange (..), File (..), TaskData (..))
import Monocle.Prelude
import Monocle.Search (FieldsRequest, FieldsResponse (..), QueryRequest, QueryResponse)
import qualified Monocle.Search as SearchPB
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import Monocle.Servant.Env
import qualified Monocle.TaskData as TaskDataPB
import qualified Network.Wai.Middleware.Auth as Auth
import qualified Network.Wai.Middleware.Auth.Provider as Auth
import Proto3.Suite (Enumerated (..))
import Servant.API.Vault (Vault)

searchQuery :: Maybe Auth.AuthUser -> QueryRequest -> AppM QueryResponse
searchQuery auth request = do
  putTextLn $ "vaul user: " <> show auth
  Env {bhEnv = bhEnv} <- ask
  now <- liftIO getCurrentTime
  SearchPB.QueryResponse . Just <$> response bhEnv now
  where
    queryText = toStrict $ SearchPB.queryRequestQuery request
    index = "monocle.changes.1." <> toStrict (SearchPB.queryRequestIndex request)
    response bhEnv now = case P.parse queryText >>= Q.queryWithMods now of
      Left (ParseError msg offset) ->
        pure
          . SearchPB.QueryResponseResultError
          . SearchPB.QueryError (toLazy msg)
          $ (fromInteger . toInteger $ offset)
      Right query ->
        SearchPB.QueryResponseResultItems
          . SearchPB.Changes
          . V.fromList
          . map toResult
          <$> Q.changes bhEnv index query
    toResult :: ELKChange -> SearchPB.Change
    toResult change =
      let changeTitle = elkchangeTitle change
          changeUrl = elkchangeUrl change
          changeCreatedAt = (Just . Timestamp.fromUTCTime $ elkchangeCreatedAt change)
          changeUpdatedAt = (Just . Timestamp.fromUTCTime $ elkchangeUpdatedAt change)
          changeRepositoryFullname = elkchangeRepositoryFullname change
          changeState = elkchangeState change
          changeBranch = elkchangeBranch change
          changeTargetBranch = elkchangeTargetBranch change
          changeTaskData = V.fromList . maybe [] (map toTaskData) $ elkchangeTasksData change
          changeChangeId = elkchangeChangeId change
          changeAuthor = authorMuid . elkchangeAuthor $ change
          changeText = elkchangeText change
          changeAdditions = elkchangeAdditions change
          changeDeletions = elkchangeDeletions change
          changeChangedFilesCount = elkchangeChangedFilesCount change
          changeApproval = V.fromList $ fromMaybe [] $ elkchangeApproval change
          changeAssignees = V.fromList (fmap authorMuid (elkchangeAssignees change))
          changeLabels = V.fromList $ elkchangeLabels change
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
    toTaskData :: TaskData -> TaskDataPB.TaskData
    toTaskData td =
      let taskDataUpdatedAt = Nothing
          taskDataChangeUrl = toLazy $ tdUrl td
          taskDataTtype = fmap toLazy $ V.fromList $ tdTtype td
          taskDataTid = toLazy $ tdTid td
          taskDataUrl = toLazy $ tdUrl td
          taskDataTitle = toLazy $ tdUrl td
          taskDataSeverity = toLazy $ tdSeverity td
          taskDataPriority = toLazy $ tdPriority td
          taskDataScore = fromInteger $ toInteger $ tdScore td
       in TaskDataPB.TaskData {..}

searchFields :: Maybe Auth.AuthUser -> FieldsRequest -> AppM FieldsResponse
searchFields _auth = const $ pure response
  where
    response :: FieldsResponse
    response = FieldsResponse . V.fromList . map toResult $ Q.fields
    toResult (name, (fieldType', _realname, desc)) =
      let fieldName = toLazy name
          fieldDescription = toLazy desc
          fieldType = Enumerated . Right $ fieldType'
       in SearchPB.Field {..}

searchChangesLifecycle :: Text -> Text -> AppM SearchPB.ChangesLifecycle
searchChangesLifecycle indexName queryText = do
  Env {bhEnv = bhEnv} <- ask
  now <- liftIO getCurrentTime
  response bhEnv now
  where
    index = BH.IndexName $ "monocle.changes.1." <> indexName
    response bhEnv now = case P.parse queryText >>= Q.queryWithMods now of
      Left (ParseError msg _offset) -> error ("Oops: " <> show msg)
      Right query -> do
        let -- Helper functions ready to be applied
            bhQuery = Q.queryBH query
            count = Q.countEvents bhEnv index
            queryType = Q.documentType bhQuery

        -- get events count
        eventCounts <- Q.getEventCounts bhEnv index bhQuery

        -- histos
        let histo = Q.getHistoEventAgg bhEnv index
            histos =
              toHisto
                <$> (histo $ queryType "ChangeCreatedEvent")
                <*> (histo $ queryType "ChangeMergedEvent")
                <*> (histo $ queryType "ChangeAbandonedEvent")
                <*> (histo $ queryType "ChangeCommitPushedEvent")
                <*> (histo $ queryType "ChangeCommitForcePushedEvent")

        -- ratios
        let ratios =
              toRatio eventCounts
                <$> (count $ queryType "ChangeCreatedEvent")
                <*> (count $ queryType "ChangeCommitPushedEvent")
                <*> (count $ queryType "ChangeCommitForcePushedEvent")

        -- duration aggregate
        let durationAgg =
              Q.changeMergedStatsDuration bhEnv index bhQuery

        -- create final result
        let result =
              toResult eventCounts
                <$> ratios
                <*> histos
                <*> durationAgg

        result

    toHisto ::
      Q.HistoEventAgg ->
      Q.HistoEventAgg ->
      Q.HistoEventAgg ->
      Q.HistoEventAgg ->
      Q.HistoEventAgg ->
      SearchPB.ChangesHistos
    toHisto createdEvent mergedEvent abandonedEvent pushedEvent forcePushedEvent =
      let toEvents qe =
            let changesHistos_EventDocCount = Q.heCount qe
                changesHistos_EventKey = Q.heKey qe
                -- do we need that?
                changesHistos_EventKeyAsString = mempty
             in SearchPB.ChangesHistos_Event {..}

          changesHistosChangeAbandonedEvent = toEvents <$> Q.heBuckets abandonedEvent
          changesHistosChangeCommitForcePushedEvent = toEvents <$> Q.heBuckets forcePushedEvent
          changesHistosChangeCommitPushedEvent = toEvents <$> Q.heBuckets pushedEvent
          changesHistosChangeCreatedEvent = toEvents <$> Q.heBuckets createdEvent
          changesHistosChangeMergedEvent = toEvents <$> Q.heBuckets mergedEvent
       in SearchPB.ChangesHistos {..}

    toRatio ::
      Q.EventCounts ->
      Word32 ->
      Word32 ->
      Word32 ->
      SearchPB.ChangesLifecycle_Ratios
    toRatio Q.EventCounts {..} createdEvent pushedEvent forcePushedEvent =
      let ratio :: Word32 -> Word32 -> Deci
          ratio x y = if y == 0 then 0 else (fromInteger . toInteger $ x) / (fromInteger . toInteger $ y)
          ratioF x = fromFixed . ratio x

          changesLifecycle_RatiosMerged = mergedCount `ratioF` createdEvent
          changesLifecycle_RatiosAbandoned = abandonedCount `ratioF` createdEvent
          changesLifecycle_RatiosIterations = (pushedEvent + forcePushedEvent) `ratioF` createdEvent
          changesLifecycle_RatiosSelfMerged = selfMergedCount `ratioF` createdEvent
       in SearchPB.ChangesLifecycle_Ratios {..}

    toResult ::
      Q.EventCounts ->
      SearchPB.ChangesLifecycle_Ratios ->
      SearchPB.ChangesHistos ->
      Q.MergedStatsDuration ->
      SearchPB.ChangesLifecycle
    toResult Q.EventCounts {..} ratios histos msd =
      let changesLifecycleChangeCommitForcePushedEvent = Nothing
          changesLifecycleChangeCommitPushedEvent = Nothing
          changesLifecycleChangeCreatedEvent = Nothing
          changesLifecycleAbandoned = abandonedCount
          changesLifecycleCommits = 0
          changesLifecycleDuration = double2Float (Q.avg msd)
          changesLifecycleDurationVariability = double2Float (Q.variability msd)
          changesLifecycleHistos = Just histos
          changesLifecycleMerged = mergedCount
          changesLifecycleOpened = openedCount
          changesLifecycleRatios = Just ratios
          changesLifecycleSelfMerged = selfMergedCount
          changesLifecycleTests = 0
       in SearchPB.ChangesLifecycle {..}

searchQueryAuth :: Vault -> QueryRequest -> AppM QueryResponse
searchQueryAuth vault = searchQuery (Auth.getAuthUserVault vault)

authWhoAmI :: Vault -> AuthPB.WhoAmIRequest -> AppM AuthPB.WhoAmIResponse
authWhoAmI vault = const $ pure response
  where
    response :: AuthPB.WhoAmIResponse
    response = AuthPB.WhoAmIResponse $ toLazy $ show user
    user = fromMaybe (error "Authentication is missing") (Auth.getAuthUserFromVault vault)
