{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The servant endpoint implementation
module Monocle.Api.Server where

import Data.Fixed (Deci)
import Data.List (lookup)
import qualified Data.Vector as V
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
import qualified Monocle.UserGroup as UserGroupPB
import Proto3.Suite (Enumerated (..))

-- | /health endpoint
configHealth :: ConfigPB.HealthRequest -> AppM ConfigPB.HealthResponse
configHealth = const $ pure response
  where
    response = ConfigPB.HealthResponse "api running"

-- | /api/2/user_group/list endpoint
userGroupList :: UserGroupPB.ListRequest -> AppM UserGroupPB.ListResponse
userGroupList request = do
  Env {tenants = tenants} <- ask
  let UserGroupPB.ListRequest {..} = request

  pure . UserGroupPB.ListResponse . V.fromList $ case Config.lookupTenant tenants (toStrict listRequestIndex) of
    Just index -> toGroupCounts <$> Config.getTenantGroups index
    Nothing -> []
  where
    toGroupCounts :: (Text, [Text]) -> UserGroupPB.GroupDefinition
    toGroupCounts (name, users) =
      let groupDefinitionName = toLazy name
          groupDefinitionMembers = fromInteger . toInteger $ length users
       in UserGroupPB.GroupDefinition {..}

-- | /api/2/user_group/get endpoint
userGroupGet :: UserGroupPB.GetRequest -> AppM UserGroupPB.GetResponse
userGroupGet request = do
  Env {tenants = tenants} <- ask
  let UserGroupPB.GetRequest {..} = request
  now <- liftIO getCurrentTime

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict getRequestIndex)
            `orDie` ParseError "unknown index" 0

        users <-
          lookup (toStrict getRequestName) (Config.getTenantGroups index)
            `orDie` ParseError "unknown group" 0

        expr <- P.parse (toStrict getRequestQuery)

        query <-
          Q.queryWithMods now mempty (Just index) expr

        pure (index, users, query)

  case requestE of
    Right (index, users, query) -> runTenantQueryM index query $ getGroupStats users
    Left err -> error (show err)
  where
    getGroupStats :: [Text] -> QueryM UserGroupPB.GetResponse
    getGroupStats users = do
      let allQuery = Q.mkOr $ map Q.toUserTerm users

      allStats <- withFilter [allQuery] $ do
        UserGroupPB.GroupStat
          <$> Q.changeReviewRatio
          <*> pure 0
          <*> pure mempty

      userStats <- traverse getUserStat users

      pure $ UserGroupPB.GetResponse (Just allStats) (V.fromList userStats)

    getUserStat :: Text -> QueryM UserGroupPB.UserStat
    getUserStat name = do
      let userQuery = Q.toUserTerm name
          reviewQuery = Q.mkOr $ map (Q.mkTerm "type") ["ChangeReviewedEvent", "ChangeCommentedEvent"]

      userStats <- withFilter [userQuery] $ do
        reviewHisto <- withFilter [reviewQuery] Q.getReviewHisto

        UserGroupPB.GroupStat
          <$> Q.changeReviewRatio
          <*> pure 0
          <*> pure (toReviewHisto <$> reviewHisto)

      pure $ UserGroupPB.UserStat (toLazy name) (Just userStats)

    toReviewHisto :: Q.HistoEventBucket -> UserGroupPB.ReviewHisto
    toReviewHisto Q.HistoEventBucket {..} = UserGroupPB.ReviewHisto heKey heCount

pattern ProjectEntity project =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityProjectName project)))

toEntity :: Maybe CrawlerPB.Entity -> Entity
toEntity entityPB = case entityPB of
  ProjectEntity projectName -> Project $ toStrict projectName
  otherEntity -> error $ "Unknown Entity type: " <> show otherEntity

-- | /crawler/add endpoint
crawlerAddDoc :: CrawlerPB.AddDocRequest -> AppM CrawlerPB.AddDocResponse
crawlerAddDoc request = do
  Env {tenants = tenants} <- ask
  let (CrawlerPB.AddDocRequest indexName crawlerName apiKey entity changes events) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownIndex

        _crawler <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownCrawler

        when
          (Config.crawlers_api_key index /= Just (toStrict apiKey))
          (Left CrawlerPB.AddDocErrorAddUnknownApiKey)

        pure index

  case requestE of
    Right index -> runTenantM index $ case toEntity entity of
      Project _ -> addChanges crawlerName changes events
      Organization _ -> error "Organization entity not yet supported"
    Left err -> pure $ toErrorResponse err
  where
    addChanges crawlerName changes events = do
      monocleLogEvent $ AddingChange crawlerName (length changes) (length events)
      I.indexChanges (map I.toELKChange $ toList changes)
      I.indexEvents (map I.toELKChangeEvent $ toList events)
      pure $ CrawlerPB.AddDocResponse Nothing

    toErrorResponse :: CrawlerPB.AddDocError -> CrawlerPB.AddDocResponse
    toErrorResponse err =
      CrawlerPB.AddDocResponse
        . Just
        . CrawlerPB.AddDocResponseResultError
        . Enumerated
        $ Right err

-- | /crawler/commit endpoint
crawlerCommit :: CrawlerPB.CommitRequest -> AppM CrawlerPB.CommitResponse
crawlerCommit request = do
  Env {tenants = tenants} <- ask
  let (CrawlerPB.CommitRequest indexName crawlerName apiKey entityPB timestampM) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownIndex

        _crawler <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownCrawler

        when
          (Config.crawlers_api_key index /= Just (toStrict apiKey))
          (Left CrawlerPB.CommitErrorCommitUnknownApiKey)

        ts <-
          timestampM
            `orDie` CrawlerPB.CommitErrorCommitDateMissing

        pure (index, ts, toEntity entityPB)

  case requestE of
    Right (index, ts, entity) -> runTenantM index $ do
      let date = Timestamp.toUTCTime ts
      monocleLogEvent $ UpdatingEntity crawlerName entity date

      _ <- I.setLastUpdated (toStrict crawlerName) date entity

      pure . CrawlerPB.CommitResponse . Just $
        CrawlerPB.CommitResponseResultTimestamp ts
    Left err -> pure . toErrorResponse $ err
  where
    toErrorResponse :: CrawlerPB.CommitError -> CrawlerPB.CommitResponse
    toErrorResponse err =
      CrawlerPB.CommitResponse
        . Just
        . CrawlerPB.CommitResponseResultError
        . Enumerated
        $ Right err

-- | /crawler/get_commit_info endpoint
crawlerCommitInfo :: CrawlerPB.CommitInfoRequest -> AppM CrawlerPB.CommitInfoResponse
crawlerCommitInfo request = do
  Env {tenants = tenants} <- ask
  let (CrawlerPB.CommitInfoRequest indexName crawlerName entity) = request
      entityType = fromPBEnum entity

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownIndex

        worker <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownCrawler

        pure (index, worker)

  case requestE of
    Right (index, worker) -> runTenantM index $ do
      (name, ts) <- I.getLastUpdated worker entityType
      pure
        . CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultEntity
        . CrawlerPB.CommitInfoResponse_OldestEntity (Just $ fromEntityType entityType (toLazy name))
        $ Just (Timestamp.fromUTCTime ts)
    Left err ->
      pure $ toErrorResponse err
  where
    fromEntityType :: CrawlerPB.CommitInfoRequest_EntityType -> LText -> CrawlerPB.Entity
    fromEntityType enum value = CrawlerPB.Entity . Just $ case enum of
      CrawlerPB.CommitInfoRequest_EntityTypeOrganization -> CrawlerPB.EntityEntityOrganizationName value
      CrawlerPB.CommitInfoRequest_EntityTypeProject -> CrawlerPB.EntityEntityProjectName value
      otherEntity -> error $ "Not implemented: " <> show otherEntity

    toErrorResponse :: CrawlerPB.CommitInfoError -> CrawlerPB.CommitInfoResponse
    toErrorResponse err =
      CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultError
        . Enumerated
        $ Right err

-- | /search/query endpoint
searchQuery :: QueryRequest -> AppM QueryResponse
searchQuery request = do
  Env {tenants = tenants} <- ask
  let (SearchPB.QueryRequest {..}) = request
  now <- liftIO getCurrentTime

  let requestE =
        do
          expr <- P.parse (toStrict queryRequestQuery)

          tenant <-
            Config.lookupTenant tenants (toStrict queryRequestIndex)
              `orDie` ParseError "unknown tenant" 0

          query <-
            Q.queryWithMods now (toStrict queryRequestUsername) (Just tenant) expr

          pure (tenant, query, fromPBEnum queryRequestQueryType)

  case requestE of
    Right (tenant, query, queryType) -> runTenantQueryM tenant query $ do
      liftTenantM $ monocleLogEvent $ Searching queryType queryRequestQuery query

      case queryType of
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGE ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultChanges
            . SearchPB.Changes
            . V.fromList
            . map toResult
            <$> Q.changes
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGE_LIFECYCLE ->
          error "LifeCycle Not Implemented"
    Left err -> pure . handleError $ err
  where
    handleError :: ParseError -> SearchPB.QueryResponse
    handleError (ParseError msg offset) =
      SearchPB.QueryResponse . Just
        . SearchPB.QueryResponseResultError
        $ SearchPB.QueryError
          (toLazy msg)
          (fromInteger . toInteger $ offset)

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

lookupTenant :: Text -> AppM (Maybe Config.Index)
lookupTenant name = do
  Env {tenants = tenants} <- ask
  pure $ Config.lookupTenant tenants name

searchChangesLifecycle :: Text -> Text -> AppM SearchPB.ChangesLifecycle
searchChangesLifecycle indexName queryText = do
  now <- liftIO getCurrentTime
  tenantM <- lookupTenant indexName
  case tenantM of
    Nothing -> error $ "Unknown tenant: " <> indexName
    Just tenant -> runTenantM tenant $ response now
  where
    -- TODO: add field to the protobuf message
    username = mempty

    response now = case P.parse queryText >>= Q.queryWithMods now username Nothing of
      Left (ParseError msg _offset) -> error ("Oops: " <> show msg)
      Right query -> do
        let -- Helper functions ready to be applied
            bhQuery = fromMaybe (error "Need query") $ Q.queryBH query
            count = Q.countEvents
            queryType = Q.documentType [bhQuery]

        -- get events count
        eventCounts <- Q.getEventCounts bhQuery

        -- histos
        let histo = Q.getHistoEventAgg
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
              Q.changeMergedStatsDuration bhQuery

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
