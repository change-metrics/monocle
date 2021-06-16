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
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents (Author (..), Commit (..), ELKChange (..), File (..), TaskData (..))
import Monocle.Backend.Index as I
import qualified Monocle.Backend.Queries as Q
import qualified Monocle.Config as ConfigPB
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Prelude
import Monocle.Search (FieldsRequest, FieldsResponse (..), QueryRequest, QueryResponse)
import qualified Monocle.Search as SearchPB
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import Monocle.Servant.Env
import qualified Monocle.TaskData as TaskDataPB
import Proto3.Suite (Enumerated (..))

configHealth :: ConfigPB.HealthRequest -> AppM ConfigPB.HealthResponse
configHealth = const $ pure response
  where
    response = ConfigPB.HealthResponse "api running"

-- Mock to bind to function from another PR
crawlerExist indexName crawlerName = True

apiKeyMatch indexName crawlerName apiKey = True

crawlerAddDoc :: CrawlerPB.AddDocRequest -> AppM CrawlerPB.AddDocResponse
crawlerAddDoc request = do
  Env {bhEnv = bhEnv, tenants = tenants} <- ask
  let (CrawlerPB.AddDocRequest indexName crawlerName apiKey entity changes events) = request
  case (Config.lookupTenant tenants $ toStrict indexName, crawlerExist indexName crawlerName, apiKeyMatch indexName crawlerName apiKey) of
    (Just _, True, True) -> do
      case toEntity entity of
        I.Project _ -> do
          _ <- liftIO $ I.indexChanges bhEnv (BH.IndexName $ toStrict indexName) (map I.toELKChange $ toList changes)
          _ <- liftIO $ I.indexEvents bhEnv (BH.IndexName $ toStrict indexName) (map I.toELKChangeEvent $ toList events)
          pure $ CrawlerPB.AddDocResponse Nothing
        I.Organization _ -> error "Organization entity not yet supported"
    (Just _, True, False) -> do
      pure $ toErrorResponse CrawlerPB.AddDocErrorAddUnknownApiKey
    (Just _, False, _) -> do
      pure $ toErrorResponse CrawlerPB.AddDocErrorAddUnknownCrawler
    (Nothing, _, _) -> do
      pure $ toErrorResponse CrawlerPB.AddDocErrorAddUnknownIndex
  where
    toEntity :: Maybe CrawlerPB.AddDocRequestEntity -> I.Entity
    toEntity entityPB = case entityPB of
      Just (CrawlerPB.AddDocRequestEntityChangeEntity (CrawlerPB.ChangeEntity projectName)) -> I.Project $ toStrict projectName
      _ -> error "Unknown Entity type"
    toErrorResponse :: CrawlerPB.AddDocError -> CrawlerPB.AddDocResponse
    toErrorResponse err =
      CrawlerPB.AddDocResponse
        . Just
        . CrawlerPB.AddDocResponseResultError
        . Enumerated
        $ Right err

crawlerCommit :: CrawlerPB.CommitRequest -> AppM CrawlerPB.CommitResponse
crawlerCommit request = do
  Env {bhEnv = bhEnv, tenants = tenants} <- ask
  let (CrawlerPB.CommitRequest indexName crawlerName apiKey entity timestampM) = request
  case (Config.lookupTenant tenants (toStrict indexName), crawlerExist indexName crawlerName, apiKeyMatch indexName crawlerName apiKey, timestampM) of
    (Just _, True, True, Just ts) -> do
      _ <- liftIO $ I.setLastUpdated bhEnv (BH.IndexName $ toStrict indexName) (toEntity entity) (Timestamp.toUTCTime ts)
      pure $ CrawlerPB.CommitResponse (Just (CrawlerPB.CommitResponseResultTimestamp ts))
    (Just _, True, True, Nothing) -> do
      pure $ toErrorResponse CrawlerPB.CommitErrorCommitDateMissing
    (Just _, True, False, _) -> do
      pure $ toErrorResponse CrawlerPB.CommitErrorCommitUnknownApiKey
    (Just _, False, _, _) -> do
      pure $ toErrorResponse CrawlerPB.CommitErrorCommitUnknownCrawler
    (Nothing, _, _, _) -> do
      pure $ toErrorResponse CrawlerPB.CommitErrorCommitUnknownIndex
  where
    toEntity :: Maybe CrawlerPB.CommitRequestEntity -> I.Entity
    toEntity entityPB = case entityPB of
      Just (CrawlerPB.CommitRequestEntityChanges (CrawlerPB.ChangeEntity projectName)) -> I.Project $ toStrict projectName
      _ -> error "Unknown Entity type"
    toErrorResponse :: CrawlerPB.CommitError -> CrawlerPB.CommitResponse
    toErrorResponse err =
      CrawlerPB.CommitResponse
        . Just
        . CrawlerPB.CommitResponseResultError
        . Enumerated
        $ Right err

crawlerCommitInfo :: CrawlerPB.CommitInfoRequest -> AppM CrawlerPB.CommitInfoResponse
crawlerCommitInfo request = do
  Env {bhEnv = bhEnv, tenants = tenants} <- ask
  let (CrawlerPB.CommitInfoRequest indexName crawlerName entity) = request
  case (Config.lookupTenant tenants (toStrict indexName), crawlerExist indexName crawlerName) of
    (Just _, True) -> do
      ts <- liftIO $ I.getLastUpdated bhEnv (BH.IndexName $ toStrict indexName) (toEntity entity)
      pure
        . CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultLastCommitAt
        $ Timestamp.fromUTCTime ts
    (Just _, False) -> do
      pure $ toErrorResponse CrawlerPB.CommitInfoErrorCommitGetUnknownCrawler
    (Nothing, _) -> do
      pure $ toErrorResponse CrawlerPB.CommitInfoErrorCommitGetUnknownIndex
  where
    toEntity :: Maybe CrawlerPB.CommitInfoRequestEntity -> I.Entity
    toEntity entityPB = case entityPB of
      Just (CrawlerPB.CommitInfoRequestEntityChanges (CrawlerPB.ChangeEntity projectName)) -> I.Project $ toStrict projectName
      _ -> error "Unknown Entity type"
    toErrorResponse :: CrawlerPB.CommitInfoError -> CrawlerPB.CommitInfoResponse
    toErrorResponse err =
      CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultError
        . Enumerated
        $ Right err

searchQuery :: QueryRequest -> AppM QueryResponse
searchQuery request = do
  Env {tenants = tenants} <- ask
  now <- liftIO getCurrentTime
  toResponse <$> handleValidation (validateRequest tenants now)
  where
    queryText = toStrict $ SearchPB.queryRequestQuery request
    indexName = toStrict $ SearchPB.queryRequestIndex request
    index = "monocle.changes.1." <> indexName

    -- TODO: add field to the protobuf message
    username = mempty

    toResponse :: SearchPB.QueryResponseResult -> QueryResponse
    toResponse = SearchPB.QueryResponse . Just

    -- If the validation is a Left, use handleError, otherwise use go
    -- either :: (a -> c) -> (b -> c) -> Either a b -> c
    handleValidation :: Either ParseError Q.Query -> AppM SearchPB.QueryResponseResult
    handleValidation = either (pure . handleError) go

    handleError :: ParseError -> SearchPB.QueryResponseResult
    handleError (ParseError msg offset) =
      SearchPB.QueryResponseResultError $
        SearchPB.QueryError
          (toLazy msg)
          (fromInteger . toInteger $ offset)

    validateRequest :: [Config.Index] -> UTCTime -> Either ParseError Q.Query
    validateRequest tenants now = do
      -- Note: the bind (>>=) of Either stops when the value is a Left.
      expr <- P.parse queryText
      tenant <- case Config.lookupTenant tenants indexName of
        Nothing -> Left $ ParseError "unknown tenant" 0
        Just tenant -> Right tenant
      Q.queryWithMods now username (Just tenant) expr

    go :: Q.Query -> AppM SearchPB.QueryResponseResult
    go query = do
      Env {bhEnv = bhEnv} <- ask
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

searchChangesLifecycle :: Text -> Text -> AppM SearchPB.ChangesLifecycle
searchChangesLifecycle indexName queryText = do
  Env {bhEnv = bhEnv} <- ask
  now <- liftIO getCurrentTime
  response bhEnv now
  where
    index = BH.IndexName $ "monocle.changes.1." <> indexName

    -- TODO: add field to the protobuf message
    username = mempty

    response bhEnv now = case P.parse queryText >>= Q.queryWithMods now username Nothing of
      Left (ParseError msg _offset) -> error ("Oops: " <> show msg)
      Right query -> do
        let -- Helper functions ready to be applied
            bhQuery = fromMaybe (error "Need query") $ Q.queryBH query
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
