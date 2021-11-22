-- | The servant endpoint implementation.
-- This module provides an interface between the backend and the frontend
module Monocle.Api.Server where

import Data.List (lookup)
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import qualified Google.Protobuf.Timestamp as T
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents
  ( EChange (..),
    EChangeEvent (..),
  )
import Monocle.Backend.Index as I
import qualified Monocle.Backend.Queries as Q
import qualified Monocle.Config as ConfigPB
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Env
import Monocle.Prelude
import qualified Monocle.Project as ProjectPB
import Monocle.Search (FieldsRequest, FieldsResponse (..), QueryRequest, QueryResponse)
import qualified Monocle.Search as SearchPB
import qualified Monocle.Search.Parser as P
import Monocle.Search.Query (RangeFlavor (..))
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import qualified Monocle.UserGroup as UserGroupPB
import Monocle.Version (version)
import Proto3.Suite (Enumerated (..))

-- | 'getConfig' reload the config automatically from the env
getConfig :: AppM Config.Config
getConfig = do
  loadConfig <- asks config
  (reloaded, config) <- liftIO loadConfig
  when reloaded $ ensureWorkspacesCrawlersMD (Config.getWorkspaces config)
  pure config
  where
    ensureWorkspacesCrawlersMD workspaces = traverse_ ensureWorkspaceCrawlerMD workspaces
    ensureWorkspaceCrawlerMD workspace = do
      traverse_ (initCrawlerMD workspace) $ Config.crawlers workspace
    initCrawlerMD workspace crawler = runEmptyQueryM workspace $ I.initCrawlerMetadata crawler

-- | 'askWorkspaces' reload the workspaces automatically from the env
askWorkspaces :: AppM [Config.Index]
askWorkspaces = Config.getWorkspaces <$> getConfig

-- | /api/2/about endpoint
configGetAbout :: ConfigPB.GetAboutRequest -> AppM ConfigPB.GetAboutResponse
configGetAbout = const response
  where
    response = do
      config <- getConfig
      let aboutVersion = toLText version
          links = maybe [] Config.links (Config.about config)
          aboutLinks = fromList $ toLink <$> links
      pure $ ConfigPB.GetAboutResponse $ Just ConfigPB.About {..}
    toLink :: Config.Link -> ConfigPB.About_AboutLink
    toLink Config.Link {..} =
      let about_AboutLinkName = toLazy name
          about_AboutLinkUrl = toLazy url
          about_AboutLinkCategory = toLazy $ fromMaybe "About" category
       in ConfigPB.About_AboutLink {..}

-- | /api/2/get_workspaces endpoint
configGetWorkspaces :: ConfigPB.GetWorkspacesRequest -> AppM ConfigPB.GetWorkspacesResponse
configGetWorkspaces = const response
  where
    response = do
      tenants <- askWorkspaces
      pure . ConfigPB.GetWorkspacesResponse . V.fromList $ map toWorkspace tenants
    toWorkspace Config.Index {..} =
      let workspaceName = toLazy name
       in ConfigPB.Workspace {..}

-- | /api/2/user_group/list endpoint
userGroupList :: UserGroupPB.ListRequest -> AppM UserGroupPB.ListResponse
userGroupList request = do
  tenants <- askWorkspaces
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

-- | /api/2/get_projects
configGetProjects :: ConfigPB.GetProjectsRequest -> AppM ConfigPB.GetProjectsResponse
configGetProjects ConfigPB.GetProjectsRequest {..} = do
  tenants <- askWorkspaces
  pure . ConfigPB.GetProjectsResponse . V.fromList $ case Config.lookupTenant tenants (toStrict getProjectsRequestIndex) of
    Just index -> maybe [] (fmap toResp) (Config.projects index)
    Nothing -> []
  where
    toResp :: Config.Project -> ConfigPB.ProjectDefinition
    toResp Config.Project {..} =
      let projectDefinitionName = toLazy name
          projectDefinitionRepositoryRegex = toLazy $ fromMaybe "" repository_regex
          projectDefinitionBranchRegex = toLazy $ fromMaybe "" branch_regex
          projectDefinitionFileRegex = toLazy $ fromMaybe "" file_regex
       in ConfigPB.ProjectDefinition {..}

-- | /api/2/user_group/get endpoint
userGroupGet :: UserGroupPB.GetRequest -> AppM UserGroupPB.GetResponse
userGroupGet request = do
  tenants <- askWorkspaces
  let UserGroupPB.GetRequest {..} = request
  now <- getCurrentTime

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict getRequestIndex)
            `orDie` ParseError "unknown index" 0

        users <-
          lookup (toStrict getRequestName) (Config.getTenantGroups index)
            `orDie` ParseError "unknown group" 0

        expr <- P.parse (Q.loadAliases' index) (toStrict getRequestQuery)

        query <-
          Q.queryWithMods now mempty index expr

        -- Date histogram needs explicit bound to be set:
        let queryWithBound = Q.ensureMinBound query

        pure (index, users, queryWithBound)

  case requestE of
    Right (index, users, query) -> runQueryM index query $ getGroupStats users
    Left err -> error (show err)
  where
    getGroupStats :: [Text] -> QueryM UserGroupPB.GetResponse
    getGroupStats users = do
      let allQuery = mkOr $ map Q.toUserTerm users

      allStats <- withFilter [allQuery] $ do
        UserGroupPB.GroupStat
          <$> Q.changeReviewRatio
          <*> pure 0
          <*> pure mempty
          <*> pure mempty

      userStats <- traverse getUserStat users

      pure $ UserGroupPB.GetResponse (Just allStats) (V.fromList userStats)

    getUserStat :: Text -> QueryM UserGroupPB.UserStat
    getUserStat name = do
      let userQuery = Q.toUserTerm name
          reviewQuery = mkOr $ map (mkTerm "type") ["ChangeReviewedEvent", "ChangeCommentedEvent"]
          commitQuery =
            mkOr $
              map
                (mkTerm "type")
                [ "ChangeCommitPushedEvent",
                  "ChangeCommitForcePushedEvent"
                ]

      userStats <- withFilter [userQuery] $ do
        reviewHisto <- withFilter [reviewQuery] $ Q.getHisto CreatedAt
        commitHisto <- withFilter [commitQuery] $ Q.getHisto CreatedAt

        UserGroupPB.GroupStat
          <$> Q.changeReviewRatio
          <*> pure 0
          <*> pure (toReviewHisto <$> commitHisto)
          <*> pure (toReviewHisto <$> reviewHisto)

      pure $ UserGroupPB.UserStat (toLazy name) (Just userStats)

    toReviewHisto :: Q.HistoSimple -> UserGroupPB.ReviewHisto
    toReviewHisto Q.HistoBucket {..} = UserGroupPB.ReviewHisto hbKey hbCount

pattern ProjectEntity project =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityProjectName project)))
pattern OrganizationEntity organization =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityOrganizationName organization)))
pattern TDEntity td =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityTdName td)))

toEntity :: Maybe CrawlerPB.Entity -> Entity
toEntity entityPB = case entityPB of
  ProjectEntity projectName -> Project $ toStrict projectName
  OrganizationEntity organizationName -> Organization $ toStrict organizationName
  TDEntity tdName -> TaskDataEntity $ toStrict tdName
  otherEntity -> error $ "Unknown Entity type: " <> show otherEntity

-- | /crawler/add endpoint
crawlerAddDoc :: CrawlerPB.AddDocRequest -> AppM CrawlerPB.AddDocResponse
crawlerAddDoc request = do
  tenants <- askWorkspaces
  let ( CrawlerPB.AddDocRequest
          indexName
          crawlerName
          apiKey
          entity
          changes
          events
          projects
          taskDatas
        ) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownIndex

        crawler <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownCrawler

        when
          (Config.crawlers_api_key index /= Just (toStrict apiKey))
          (Left CrawlerPB.AddDocErrorAddUnknownApiKey)

        pure (index, crawler)

  case requestE of
    Right (index, crawler) -> runEmptyQueryM index $ case toEntity entity of
      Project _ -> addChanges crawlerName changes events
      Organization organizationName -> addProjects crawler organizationName projects
      TaskDataEntity _ -> addTDs crawlerName taskDatas
    Left err -> pure $ toErrorResponse err
  where
    addTDs crawlerName taskDatas = do
      monocleLogEvent $ AddingTaskData crawlerName (length taskDatas)
      I.taskDataAdd (toText crawlerName) $ toList taskDatas
      pure $ CrawlerPB.AddDocResponse Nothing
    addChanges crawlerName changes events = do
      monocleLogEvent $ AddingChange crawlerName (length changes) (length events)
      let changes' = map from $ toList changes
          events' = map I.toEChangeEvent $ toList events
      I.indexChanges changes'
      I.indexEvents events'
      I.updateChangesAndEventsFromOrphanTaskData changes' events'
      pure $ CrawlerPB.AddDocResponse Nothing
    addProjects crawler organizationName projects = do
      monocleLogEvent $ AddingProject (getWorkerName crawler) organizationName (length projects)
      I.initCrawlerEntities (Project <$> projectNames projects) crawler
      pure $ CrawlerPB.AddDocResponse Nothing
    projectNames projectsV = toList (toText . ProjectPB.projectFullPath <$> projectsV)

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
  tenants <- askWorkspaces
  let (CrawlerPB.CommitRequest indexName crawlerName apiKey entityPB timestampM) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownIndex

        _crawler <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownCrawler

        when
          (Config.crawlers_api_key index /= (Just $ toStrict apiKey))
          (Left CrawlerPB.CommitErrorCommitUnknownApiKey)

        ts <-
          timestampM
            `orDie` CrawlerPB.CommitErrorCommitDateMissing

        pure (index, ts, toEntity entityPB)

  case requestE of
    Right (index, ts, entity) -> runEmptyQueryM index $ do
      let date = Timestamp.toUTCTime ts
      monocleLogEvent $ UpdatingEntity crawlerName entity date
      -- TODO: check for CommitDateInferiorThanPrevious
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
  tenants <- askWorkspaces
  let (CrawlerPB.CommitInfoRequest indexName crawlerName entityM offset) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (toStrict indexName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownIndex

        worker <-
          Config.lookupCrawler index (toStrict crawlerName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownCrawler

        pure (index, worker, entityM)

  case requestE of
    Right (index, worker, Just (CrawlerPB.Entity (Just entity))) -> runEmptyQueryM index $ do
      toUpdateEntityM <- I.getLastUpdated worker entity offset
      case toUpdateEntityM of
        Just (name, ts) ->
          pure
            . CrawlerPB.CommitInfoResponse
            . Just
            . CrawlerPB.CommitInfoResponseResultEntity
            . CrawlerPB.CommitInfoResponse_OldestEntity (Just $ fromEntityType entity (toLazy name))
            $ Just (Timestamp.fromUTCTime ts)
        Nothing -> pure . toErrorResponse $ CrawlerPB.CommitInfoErrorCommitGetNoEntity
    Right _ -> error $ "Unknown entity request: " <> show entityM
    Left err ->
      pure $ toErrorResponse err
  where
    fromEntityType :: CrawlerPB.EntityEntity -> LText -> CrawlerPB.Entity
    fromEntityType enum value = CrawlerPB.Entity . Just $ case enum of
      CrawlerPB.EntityEntityOrganizationName _ -> CrawlerPB.EntityEntityOrganizationName value
      CrawlerPB.EntityEntityProjectName _ -> CrawlerPB.EntityEntityProjectName value
      CrawlerPB.EntityEntityTdName _ -> CrawlerPB.EntityEntityTdName value

    toErrorResponse :: CrawlerPB.CommitInfoError -> CrawlerPB.CommitInfoResponse
    toErrorResponse err =
      CrawlerPB.CommitInfoResponse
        . Just
        . CrawlerPB.CommitInfoResponseResultError
        . Enumerated
        $ Right err

-- | /task_data endpoints
data TDError
  = TDUnknownApiKey
  | TDUnknownCrawler
  | TDUnknownIndex
  | TDDateInvalid
  | TDAddLenExcedeed
  deriving (Show)

validateTaskDataRequest ::
  -- | The index name
  LText ->
  -- | The crawler name
  LText ->
  -- | The api key
  Maybe LText ->
  -- | True if the timestamp must be checked
  Bool ->
  -- | the commit timestamp
  Maybe T.Timestamp ->
  -- | an AppM with either an Error or extracted info
  AppM (Either TDError (Config.Index, Config.Crawler, Maybe UTCTime))
validateTaskDataRequest indexName crawlerName apiKey checkCommitDate commitDate = do
  tenants <- askWorkspaces
  pure $ do
    index <- Config.lookupTenant tenants (toStrict indexName) `orDie` TDUnknownIndex
    crawler <- Config.lookupCrawler index (toStrict crawlerName) `orDie` TDUnknownCrawler
    when (isJust apiKey) $
      when (Config.crawlers_api_key index /= (toStrict <$> apiKey)) (Left TDUnknownApiKey)
    when checkCommitDate $
      void commitDate `orDie` TDDateInvalid
    pure (index, crawler, T.toUTCTime <$> commitDate)

-- | /suggestions endpoint
searchSuggestions :: SearchPB.SuggestionsRequest -> AppM SearchPB.SuggestionsResponse
searchSuggestions request = do
  tenants <- askWorkspaces
  let SearchPB.SuggestionsRequest {..} = request

  let tenantM = Config.lookupTenant tenants (toStrict suggestionsRequestIndex)

  case tenantM of
    Just tenant -> do
      now <- getCurrentTime
      runQueryM tenant (emptyQ now) $ Q.getSuggestions tenant
    Nothing ->
      -- Simply return empty suggestions in case of unknown tenant
      pure $
        SearchPB.SuggestionsResponse mempty mempty mempty mempty mempty mempty mempty mempty
  where
    emptyQ now' = Q.blankQuery now' $ Q.yearAgo now'

-- | A helper function to decode search query
validateSearchRequest :: LText -> LText -> LText -> AppM (Either ParseError (Config.Index, Q.Query))
validateSearchRequest tenantName queryText username = do
  tenants <- askWorkspaces
  now <- getCurrentTime
  let requestE =
        do
          tenant <-
            Config.lookupTenant tenants (toStrict tenantName)
              `orDie` ParseError "unknown tenant" 0

          expr <- P.parse (Q.loadAliases' tenant) (toStrict queryText)

          query <-
            Q.queryWithMods now (toStrict username) tenant expr

          pure (tenant, query)

  pure requestE

-- | /search/check endpoint
searchCheck :: SearchPB.CheckRequest -> AppM SearchPB.CheckResponse
searchCheck request = do
  let SearchPB.CheckRequest {..} = request

  incCounter monocleQueryCheckCounter
  requestE <- validateSearchRequest checkRequestIndex checkRequestQuery checkRequestUsername

  pure $
    SearchPB.CheckResponse $
      Just $ case requestE of
        Right _ -> SearchPB.CheckResponseResultSuccess "ok"
        Left (ParseError msg offset) ->
          SearchPB.CheckResponseResultError $
            SearchPB.QueryError (toLazy msg) (fromInteger . toInteger $ offset)

-- | /search/query endpoint
searchQuery :: QueryRequest -> AppM QueryResponse
searchQuery request = do
  let SearchPB.QueryRequest {..} = request

  incCounter monocleQueryCounter
  requestE <- validateSearchRequest queryRequestIndex queryRequestQuery queryRequestUsername

  case requestE of
    Right (tenant, query) -> runQueryM tenant (Q.ensureMinBound query) $ do
      let queryType = fromPBEnum queryRequestQueryType
      monocleLogEvent $ Searching queryType queryRequestQuery query

      case queryType of
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGE ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultChanges
            . SearchPB.Changes
            . V.fromList
            . map from
            <$> Q.changes queryRequestOrder queryRequestLimit
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGE_AND_EVENTS ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultChangeEvents
            . toChangeEventsResult
            <$> Q.changeEvents queryRequestChangeId queryRequestLimit
        SearchPB.QueryRequest_QueryTypeQUERY_REPOS_SUMMARY ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultReposSummary
            . SearchPB.ReposSummary
            . V.fromList
            . map toRSumResult
            <$> Q.getReposSummary
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGES_LIFECYCLE_STATS ->
          SearchPB.QueryResponse . Just . SearchPB.QueryResponseResultLifecycleStats
            <$> Q.getLifecycleStats
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGES_REVIEW_STATS ->
          SearchPB.QueryResponse . Just . SearchPB.QueryResponseResultReviewStats
            <$> Q.getReviewStats
        SearchPB.QueryRequest_QueryTypeQUERY_ACTIVE_AUTHORS_STATS ->
          SearchPB.QueryResponse . Just . SearchPB.QueryResponseResultActivityStats
            <$> Q.getActivityStats
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_COMMENTED ->
          handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeCommented
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_REVIEWED ->
          handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeReviewed
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_CREATED ->
          handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeCreated
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_MERGED ->
          handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeMerged
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_REVIEWED_AUTHORS ->
          handleTopAuthorsQ queryRequestLimit Q.getMostReviewedAuthor
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_COMMENTED_AUTHORS ->
          handleTopAuthorsQ queryRequestLimit Q.getMostCommentedAuthor
        SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_PEERS ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultAuthorsPeers
            . SearchPB.AuthorsPeers
            . V.fromList
            . map toAPeerResult
            <$> Q.getAuthorsPeersStrength queryRequestLimit
        SearchPB.QueryRequest_QueryTypeQUERY_NEW_CHANGES_AUTHORS -> do
          results <- take (fromInteger . toInteger $ queryRequestLimit) <$> Q.getNewContributors
          pure $
            SearchPB.QueryResponse . Just $
              SearchPB.QueryResponseResultNewAuthors $
                toTermsCount (V.fromList $ toTTResult <$> results) 0
        SearchPB.QueryRequest_QueryTypeQUERY_CHANGES_TOPS ->
          SearchPB.QueryResponse . Just
            . SearchPB.QueryResponseResultChangesTops
            <$> Q.getChangesTops queryRequestLimit
    Left err -> pure . handleError $ err
  where
    handleError :: ParseError -> SearchPB.QueryResponse
    handleError (ParseError msg offset) =
      SearchPB.QueryResponse . Just
        . SearchPB.QueryResponseResultError
        $ SearchPB.QueryError
          (toLazy msg)
          (fromInteger . toInteger $ offset)

    handleTopAuthorsQ :: Word32 -> (Word32 -> QueryM Q.TermsResultWTH) -> QueryM QueryResponse
    handleTopAuthorsQ limit cb = do
      results <- cb limit
      pure $
        SearchPB.QueryResponse
          . Just
          $ SearchPB.QueryResponseResultTopAuthors $
            toTermsCount (V.fromList $ toTTResult <$> Q.tsrTR results) (toInt $ Q.tsrTH results)
      where
        toInt c = fromInteger $ toInteger c

    toAPeerResult :: Q.PeerStrengthResult -> SearchPB.AuthorPeer
    toAPeerResult Q.PeerStrengthResult {..} =
      SearchPB.AuthorPeer
        (toLazy psrAuthor)
        (toLazy psrPeer)
        psrStrength

    toTermsCount :: V.Vector SearchPB.TermCount -> Word32 -> SearchPB.TermsCount
    toTermsCount tcV total =
      let termsCountTermcount = tcV
          termsCountTotalHits = total
       in SearchPB.TermsCount {..}

    toTTResult :: Q.TermResult -> SearchPB.TermCount
    toTTResult Q.TermResult {..} =
      SearchPB.TermCount
        (toLazy trTerm)
        (fromInteger $ toInteger trCount)

    toRSumResult :: Q.RepoSummary -> SearchPB.RepoSummary
    toRSumResult Q.RepoSummary {..} =
      let repoSummaryFullname = toLazy fullname
          repoSummaryCreatedChanges = countToWord createdChanges
          repoSummaryAbandonedChanges = countToWord abandonedChanges
          repoSummaryMergedChanges = countToWord mergedChanges
          repoSummaryUpdatedChanges = countToWord updatedChanges
          repoSummaryOpenChanges = countToWord openChanges
       in SearchPB.RepoSummary {..}

    toChangeEventsResult :: (EChange, [EChangeEvent]) -> SearchPB.ChangeAndEvents
    toChangeEventsResult (change, events) =
      let changeAndEventsChange = Just (from change)
          changeAndEventsEvents = V.fromList $ from <$> events
       in SearchPB.ChangeAndEvents {..}

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
  tenants <- askWorkspaces
  pure $ Config.lookupTenant tenants name
