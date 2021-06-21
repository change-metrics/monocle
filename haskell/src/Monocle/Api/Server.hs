{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The servant endpoint implementation
module Monocle.Api.Server where

import Data.Fixed (Deci)
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

-- | /health endpoint
configHealth :: ConfigPB.HealthRequest -> AppM ConfigPB.HealthResponse
configHealth = const $ pure response
  where
    response = ConfigPB.HealthResponse "api running"

pattern ProjectEntity project =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityProjectName project)))

toEntity :: Maybe CrawlerPB.Entity -> I.Entity
toEntity entityPB = case entityPB of
  ProjectEntity projectName -> I.Project $ toStrict projectName
  _ -> error "Unknown Entity type"

fromPBEnum :: Enumerated a -> a
fromPBEnum (Enumerated (Left x)) = error $ "Unknown enum value: " <> show x
fromPBEnum (Enumerated (Right x)) = x

-- | /crawler/add endpoint
crawlerAddDoc :: CrawlerPB.AddDocRequest -> AppM CrawlerPB.AddDocResponse
crawlerAddDoc request = do
  Env {bhEnv = bhEnv, tenants = tenants} <- ask
  let (CrawlerPB.AddDocRequest indexName crawlerName apiKey entity changes events) = request

  case validateRequest tenants (toStrict indexName) (toStrict crawlerName) (toStrict apiKey) of
    Right index -> case toEntity entity of
      I.Project _ -> addChanges bhEnv index changes events
      I.Organization _ -> error "Organization entity not yet supported"
    Left err -> pure $ toErrorResponse err
  where
    validateRequest :: [Config.Index] -> Text -> Text -> Text -> Either CrawlerPB.AddDocError Config.Index
    validateRequest tenants indexName crawlerName apiKey = do
      index <- Config.lookupTenant tenants indexName `orDie` CrawlerPB.AddDocErrorAddUnknownIndex
      _crawler <- Config.lookupCrawler index crawlerName `orDie` CrawlerPB.AddDocErrorAddUnknownCrawler
      when (Config.crawlers_api_key index /= Just apiKey) (Left CrawlerPB.AddDocErrorAddUnknownApiKey)
      pure index

    addChanges bhEnv index changes events = do
      let indexName' = I.tenantIndexName index
      monocleLogEvent $ AddingChange indexName' (length changes) (length events)
      liftIO $ I.indexChanges bhEnv indexName' (map I.toELKChange $ toList changes)
      liftIO $ I.indexEvents bhEnv indexName' (map I.toELKChangeEvent $ toList events)
      liftIO $ I.refreshIndex bhEnv indexName'
      pure $ CrawlerPB.AddDocResponse Nothing

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
  case validateRequest tenants (toStrict indexName) (toStrict crawlerName) (toStrict apiKey) timestampM of
    Right (index, ts') -> do
      _ <-
        liftIO $
          I.setLastUpdated
            bhEnv
            (I.tenantIndexName index)
            (toStrict crawlerName)
            (Timestamp.toUTCTime ts')
            (toEntity entity)
      pure $ CrawlerPB.CommitResponse (Just $ CrawlerPB.CommitResponseResultTimestamp ts')
    Left err -> pure $ toErrorResponse err
  where
    validateRequest :: [Config.Index] -> Text -> Text -> Text -> Maybe Timestamp -> Either CrawlerPB.CommitError (Config.Index, Timestamp)
    validateRequest tenants indexName crawlerName apiKey timestampM = do
      index <- Config.lookupTenant tenants indexName `orDie` CrawlerPB.CommitErrorCommitUnknownIndex
      _crawler <- Config.lookupCrawler index crawlerName `orDie` CrawlerPB.CommitErrorCommitUnknownCrawler
      when (Config.crawlers_api_key index /= Just apiKey) (Left CrawlerPB.CommitErrorCommitUnknownApiKey)
      ts <- timestampM `orDie` CrawlerPB.CommitErrorCommitDateMissing
      pure (index, ts)

    toErrorResponse :: CrawlerPB.CommitError -> CrawlerPB.CommitResponse
    toErrorResponse err =
      CrawlerPB.CommitResponse
        . Just
        . CrawlerPB.CommitResponseResultError
        . Enumerated
        $ Right err

-- | Returns the oldest entity
crawlerCommitInfo :: CrawlerPB.CommitInfoRequest -> AppM CrawlerPB.CommitInfoResponse
crawlerCommitInfo request = do
  Env {bhEnv = bhEnv, tenants = tenants} <- ask
  let (CrawlerPB.CommitInfoRequest indexName crawlerName entity) = request
      entityType = fromPBEnum entity
  case validateRequest tenants (toStrict indexName) (toStrict crawlerName) of
    Right (index, worker) -> do
      (name, ts) <- liftIO $ I.getLastUpdated bhEnv (I.tenantIndexName index) worker entityType
      pure
        . CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultEntity
        . CrawlerPB.CommitInfoResponse_OldestEntity (Just $ fromEntityType entityType (toLazy name))
        $ Just (Timestamp.fromUTCTime ts)
    Left err -> pure $ toErrorResponse err
  where
    validateRequest :: [Config.Index] -> Text -> Text -> Either CrawlerPB.CommitInfoError (Config.Index, Config.Crawler)
    validateRequest tenants indexName crawlerName = do
      index <- Config.lookupTenant tenants indexName `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownIndex
      worker <- Config.lookupCrawler index crawlerName `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownCrawler
      pure (index, worker)

    fromEntityType :: CrawlerPB.CommitInfoRequest_EntityType -> LText -> CrawlerPB.Entity
    fromEntityType enum value = CrawlerPB.Entity . Just $ case enum of
      CrawlerPB.CommitInfoRequest_EntityTypeOrganization -> CrawlerPB.EntityEntityOrganizationName value
      CrawlerPB.CommitInfoRequest_EntityTypeProject -> CrawlerPB.EntityEntityProjectName value
      _ -> error "Not implemented"

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
                <$> histo (queryType "ChangeCreatedEvent")
                <*> histo (queryType "ChangeMergedEvent")
                <*> histo (queryType "ChangeAbandonedEvent")
                <*> histo (queryType "ChangeCommitPushedEvent")
                <*> histo (queryType "ChangeCommitForcePushedEvent")

        -- ratios
        let ratios =
              toRatio eventCounts
                <$> count (queryType "ChangeCreatedEvent")
                <*> count (queryType "ChangeCommitPushedEvent")
                <*> count (queryType "ChangeCommitForcePushedEvent")

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
