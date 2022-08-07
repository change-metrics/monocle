-- | The servant endpoint implementation.
-- This module provides an interface between the backend and the frontend
module Monocle.Api.Server where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as AK (fromText)
import Data.Aeson.KeyMap qualified as AKM (lookup)
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Api.Jwt (
  AuthenticatedUser (aDefaultMuid, aMuidMap),
  LoginInUser (..),
  OIDCEnv (..),
  OIDCState (OIDCState),
  decodeOIDCState,
  mkJwt,
  mkSessionStore,
 )
import Monocle.Backend.Documents (
  EChange (..),
  EChangeEvent (..),
 )
import Monocle.Backend.Index as I
import Monocle.Backend.Queries qualified as Q
import Monocle.Config (Config, OIDCProviderConfig (..))
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Prelude
import Monocle.Protob.Auth qualified as AuthPB
import Monocle.Protob.Config qualified as ConfigPB
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Login qualified as LoginPB
import Monocle.Protob.Metric (GetRequest (getRequestMetric))
import Monocle.Protob.Metric qualified as MetricPB
import Monocle.Protob.Search (QueryRequest (queryRequestLimit))
import Monocle.Protob.Search qualified as SearchPB
import Monocle.Search.Parser qualified as P
import Monocle.Search.Query qualified as Q
import Monocle.Search.Syntax (ParseError (..))
import Monocle.Version (version)
import Proto3.Suite (Enumerated (..))
import Servant (
  NoContent (..),
  ServerError (errBody, errHeaders, errReasonPhrase),
  err302,
  err403,
 )
import Servant.Auth.Server (AuthResult (Authenticated))
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Web.OIDC.Client (sub)
import Web.OIDC.Client qualified as O

-- | 'getConfig' reload the config automatically from the env
getConfig :: AppM Config.ConfigStatus
getConfig = do
  loadConfig <- asks config
  liftIO loadConfig

-- | 'updateIndex' if needed - ensures index exists and refresh crawler Metadata
updateIndex :: Config.Index -> MVar Config.WorkspaceStatus -> AppM ()
updateIndex index wsRef = runEmptyQueryM index $ modifyMVar_ wsRef doUpdateIfNeeded
 where
  doUpdateIfNeeded :: Config.WorkspaceStatus -> QueryM Config.WorkspaceStatus
  doUpdateIfNeeded ws = case Map.lookup (Config.getWorkspaceName index) ws of
    Just Config.Ready -> pure ws
    Just Config.NeedRefresh -> do
      refreshIndex
      pure $ Map.insert (Config.getWorkspaceName index) Config.Ready ws
    Nothing -> error $ "Unknown workspace: " <> show (Config.getWorkspaceName index)

  refreshIndex :: QueryM ()
  refreshIndex = do
    logEvent $ RefreshIndex index
    I.ensureIndexSetup
    traverse_ I.initCrawlerMetadata $ Config.crawlers index

-- | Convenient pattern to get the config from the status
pattern GetConfig :: Config.Config -> Config.ConfigStatus
pattern GetConfig a <- Config.ConfigStatus _ a _

-- | Convenient pattern to get the list of tenants
pattern GetTenants :: [Config.Index] -> Config.ConfigStatus
pattern GetTenants a <- Config.ConfigStatus _ (Config.Config _about _auth a) _

checkAuth :: forall a. AuthResult AuthenticatedUser -> (Maybe AuthenticatedUser -> AppM a) -> AppM a
checkAuth auth action = do
  aOIDC <- asks aOIDC
  case oidcEnv aOIDC of
    Just (OIDCEnv {providerConfig}) -> case auth of
      Authenticated au -> action $ Just au
      _ -> do
        if not $ opEnforceAuth providerConfig
          then action Nothing
          else forbidden "Not allowed"
    Nothing -> action Nothing

-- curl -XPOST -d '{"void": ""}' -H "Content-type: application/json" -H 'Authorization: Bearer <token>' http://localhost:8080/auth/whoami
authWhoAmi :: AuthResult AuthenticatedUser -> AuthPB.WhoAmiRequest -> AppM AuthPB.WhoAmiResponse
authWhoAmi (Authenticated au) _request =
  pure $ AuthPB.WhoAmiResponse . Just . AuthPB.WhoAmiResponseResultUid $ show au
authWhoAmi _auth _request = pure $ AuthPB.WhoAmiResponse . Just . AuthPB.WhoAmiResponseResultError . Enumerated $ Right AuthPB.WhoAmiErrorUnAuthorized

-- curl -XPOST -d '{"token": "admin-token"}' -H "Content-type: application/json" http://localhost:8080/auth/get
authGetMagicJwt :: AuthResult AuthenticatedUser -> AuthPB.GetMagicJwtRequest -> AppM AuthPB.GetMagicJwtResponse
authGetMagicJwt _auth (AuthPB.GetMagicJwtRequest inputAdminToken) = do
  oidc <- asks aOIDC
  -- The generated JWT does not have any expiry
  -- An API restart generates new JWK that will invalidate the token
  jwtE <- liftIO $ mkJwt (localJWTSettings oidc) Map.empty "bot" Nothing
  adminTokenM <- liftIO $ lookupEnv "MONOCLE_ADMIN_TOKEN"
  case (jwtE, adminTokenM) of
    (Right jwt, Just adminToken) | inputAdminToken == from adminToken -> pure . genSuccess $ decodeUtf8 jwt
    (_, Just adminToken) | inputAdminToken /= from adminToken -> pure $ genErr AuthPB.GetMagicJwtErrorInvalidAdminToken
    (_, Nothing) -> pure $ genErr AuthPB.GetMagicJwtErrorMagicTokenDisabled
    _ -> pure $ genErr AuthPB.GetMagicJwtErrorMagicTokenCreateError
 where
  genErr :: AuthPB.GetMagicJwtError -> AuthPB.GetMagicJwtResponse
  genErr err =
    AuthPB.GetMagicJwtResponse
      . Just
      . AuthPB.GetMagicJwtResponseResultError
      . Enumerated
      $ Right err
  genSuccess :: Text -> AuthPB.GetMagicJwtResponse
  genSuccess jwt =
    AuthPB.GetMagicJwtResponse
      . Just
      . AuthPB.GetMagicJwtResponseResultJwt
      $ from jwt

-- | /login/validate endpoint
loginLoginValidation ::
  AuthResult AuthenticatedUser -> LoginPB.LoginValidationRequest -> AppM LoginPB.LoginValidationResponse
loginLoginValidation _auth request = do
  GetTenants tenants <- getConfig
  let username = request & LoginPB.loginValidationRequestUsername
  validated <- runMaybeT $ traverse (validateOnIndex $ from username) tenants
  let result =
        -- validateOnIndex uses `mzero` to indicate success, resulting in a Nothing value
        case validated of
          Nothing -> LoginPB.LoginValidationResponse_ValidationResultKnownIdent
          Just _ -> LoginPB.LoginValidationResponse_ValidationResultUnknownIdent
  pure
    . LoginPB.LoginValidationResponse
    . Just
    . LoginPB.LoginValidationResponseResultValidationResult
    . Enumerated
    $ Right result
 where
  validateOnIndex :: Text -> Config.Index -> MaybeT AppM ()
  validateOnIndex username index = do
    let userQuery = Q.toUserTerm username
    count <- lift $ runEmptyQueryM index $ withFilter [userQuery] Q.countDocs
    when (count > 0) mzero

-- | /api/2/about endpoint
configGetAbout :: AuthResult AuthenticatedUser -> ConfigPB.GetAboutRequest -> AppM ConfigPB.GetAboutResponse
configGetAbout _auth _request = response
 where
  response = do
    aOIDC <- asks aOIDC
    GetConfig config <- getConfig
    let aboutVersion = from version
        links = maybe [] Config.links (Config.about config)
        aboutLinks = fromList $ toLink <$> links
        aboutAuth = toAuth . providerConfig <$> oidcEnv aOIDC
    pure $ ConfigPB.GetAboutResponse $ Just ConfigPB.About {..}
  toLink :: Config.Link -> ConfigPB.About_AboutLink
  toLink Config.Link {..} =
    let about_AboutLinkName = from name
        about_AboutLinkUrl = from url
        about_AboutLinkCategory = from $ fromMaybe "About" category
     in ConfigPB.About_AboutLink {..}
  toAuth Config.OIDCProviderConfig {..} =
    let about_AuthConfigForceLogin = opEnforceAuth
        about_AuthConfigIssuer = from opIssuerURL
        about_AuthConfigProviderName = from opName
     in ConfigPB.AboutAuthAuthConfig $ ConfigPB.About_AuthConfig {..}

-- | /api/2/get_workspaces endpoint
configGetWorkspaces :: AuthResult AuthenticatedUser -> ConfigPB.GetWorkspacesRequest -> AppM ConfigPB.GetWorkspacesResponse
configGetWorkspaces auth _request = checkAuth auth $ const response
 where
  response = do
    GetTenants tenants <- getConfig
    pure . ConfigPB.GetWorkspacesResponse . V.fromList $ map toWorkspace tenants
  toWorkspace Config.Index {..} =
    let workspaceName = from name
     in ConfigPB.Workspace {..}

-- | /api/2/get_groups endpoint
configGetGroups :: AuthResult AuthenticatedUser -> ConfigPB.GetGroupsRequest -> AppM ConfigPB.GetGroupsResponse
configGetGroups auth request = checkAuth auth . const $ do
  GetTenants tenants <- getConfig
  let ConfigPB.GetGroupsRequest {..} = request

  pure . ConfigPB.GetGroupsResponse . V.fromList $ case Config.lookupTenant tenants (from getGroupsRequestIndex) of
    Just index -> toGroupCounts <$> Config.getTenantGroups index
    Nothing -> []
 where
  toGroupCounts :: (Text, [Text]) -> ConfigPB.GroupDefinition
  toGroupCounts (name, users) =
    let groupDefinitionName = from name
        groupDefinitionMembers = fromInteger . toInteger $ length users
     in ConfigPB.GroupDefinition {..}

-- | /api/2/get_group_members endpoint
configGetGroupMembers :: AuthResult AuthenticatedUser -> ConfigPB.GetGroupMembersRequest -> AppM ConfigPB.GetGroupMembersResponse
configGetGroupMembers auth request = checkAuth auth . const $ do
  GetTenants tenants <- getConfig
  let ConfigPB.GetGroupMembersRequest {..} = request
  members <- case Config.lookupTenant tenants (from getGroupMembersRequestIndex) of
    Just index -> pure $ fromMaybe [] $ lookup (from getGroupMembersRequestGroup) (Config.getTenantGroups index)
    Nothing -> pure []
  pure . ConfigPB.GetGroupMembersResponse . V.fromList $ from <$> members

-- | /api/2/get_projects
configGetProjects :: AuthResult AuthenticatedUser -> ConfigPB.GetProjectsRequest -> AppM ConfigPB.GetProjectsResponse
configGetProjects auth ConfigPB.GetProjectsRequest {..} = checkAuth auth . const $ do
  GetTenants tenants <- getConfig
  pure . ConfigPB.GetProjectsResponse . V.fromList $ case Config.lookupTenant tenants (from getProjectsRequestIndex) of
    Just index -> maybe [] (fmap toResp) (Config.projects index)
    Nothing -> []
 where
  toResp :: Config.Project -> ConfigPB.ProjectDefinition
  toResp Config.Project {..} =
    let projectDefinitionName = from name
        projectDefinitionRepositoryRegex = from $ fromMaybe "" repository_regex
        projectDefinitionBranchRegex = from $ fromMaybe "" branch_regex
        projectDefinitionFileRegex = from $ fromMaybe "" file_regex
     in ConfigPB.ProjectDefinition {..}

pattern ProjectEntity :: LText -> Maybe CrawlerPB.Entity
pattern ProjectEntity project =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityProjectName project)))

pattern OrganizationEntity :: LText -> Maybe CrawlerPB.Entity
pattern OrganizationEntity organization =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityOrganizationName organization)))

pattern TDEntity :: LText -> Maybe CrawlerPB.Entity
pattern TDEntity td =
  Just (CrawlerPB.Entity (Just (CrawlerPB.EntityEntityTdName td)))

toEntity :: Maybe CrawlerPB.Entity -> Entity
toEntity entityPB = case entityPB of
  ProjectEntity projectName -> Project $ from projectName
  OrganizationEntity organizationName -> Organization $ from organizationName
  TDEntity tdName -> TaskDataEntity $ from tdName
  otherEntity -> error $ "Unknown Entity type: " <> show otherEntity

-- | /crawler/add endpoint
crawlerAddDoc :: AuthResult AuthenticatedUser -> CrawlerPB.AddDocRequest -> AppM CrawlerPB.AddDocResponse
crawlerAddDoc _auth request = do
  GetTenants tenants <- getConfig
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
          Config.lookupTenant tenants (from indexName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownIndex

        crawler <-
          Config.lookupCrawler index (from crawlerName)
            `orDie` CrawlerPB.AddDocErrorAddUnknownCrawler

        when
          (Config.crawlers_api_key index /= Just (from apiKey))
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
    logEvent $ AddingTaskData crawlerName (length taskDatas)
    I.taskDataAdd (from crawlerName) $ toList taskDatas
    pure $ CrawlerPB.AddDocResponse Nothing
  addChanges crawlerName changes events = do
    logEvent $ AddingChange crawlerName (length changes) (length events)
    let changes' = map from $ toList changes
        events' = map I.toEChangeEvent $ toList events
    I.indexChanges changes'
    I.indexEvents events'
    I.updateChangesAndEventsFromOrphanTaskData changes' events'
    I.addCachedAuthors events'
    pure $ CrawlerPB.AddDocResponse Nothing
  addProjects crawler organizationName projects = do
    logEvent $ AddingProject (getWorkerName crawler) organizationName (length projects)
    let names = projectNames projects
        -- TODO(fbo) Enable crawl github issues by default for an organization.
        -- We might need to re-think some data fetching like priority/severity.
        -- entities = (Project <$> names) <> (TaskDataEntity <$> names)
        entities = Project <$> names
    I.initCrawlerEntities entities crawler
    pure $ CrawlerPB.AddDocResponse Nothing
  projectNames projectsV = toList (from . CrawlerPB.projectFullPath <$> projectsV)

  toErrorResponse :: CrawlerPB.AddDocError -> CrawlerPB.AddDocResponse
  toErrorResponse err =
    CrawlerPB.AddDocResponse
      . Just
      . CrawlerPB.AddDocResponseResultError
      . Enumerated
      $ Right err

-- | /crawler/commit endpoint
crawlerCommit :: AuthResult AuthenticatedUser -> CrawlerPB.CommitRequest -> AppM CrawlerPB.CommitResponse
crawlerCommit _auth request = do
  GetTenants tenants <- getConfig
  let (CrawlerPB.CommitRequest indexName crawlerName apiKey entityPB timestampM) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (from indexName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownIndex

        _crawler <-
          Config.lookupCrawler index (from crawlerName)
            `orDie` CrawlerPB.CommitErrorCommitUnknownCrawler

        when
          (Config.crawlers_api_key index /= Just (from apiKey))
          (Left CrawlerPB.CommitErrorCommitUnknownApiKey)

        ts <-
          timestampM
            `orDie` CrawlerPB.CommitErrorCommitDateMissing

        pure (index, ts, toEntity entityPB)

  case requestE of
    Right (index, ts, entity) -> runEmptyQueryM index $ do
      let date = Timestamp.toUTCTime ts
      logEvent $ UpdatingEntity crawlerName entity date
      -- TODO: check for CommitDateInferiorThanPrevious
      _ <- I.setLastUpdated (from crawlerName) date entity

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
crawlerCommitInfo :: AuthResult AuthenticatedUser -> CrawlerPB.CommitInfoRequest -> AppM CrawlerPB.CommitInfoResponse
crawlerCommitInfo _auth request = do
  Config.ConfigStatus _ Config.Config {..} wsStatus <- getConfig
  let tenants = workspaces
  let (CrawlerPB.CommitInfoRequest indexName crawlerName entityM offset) = request

  let requestE = do
        index <-
          Config.lookupTenant tenants (from indexName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownIndex

        worker <-
          Config.lookupCrawler index (from crawlerName)
            `orDie` CrawlerPB.CommitInfoErrorCommitGetUnknownCrawler

        pure (index, worker, entityM)

  case requestE of
    Right (index, worker, Just (CrawlerPB.Entity (Just entity))) -> do
      updateIndex index wsStatus
      runEmptyQueryM index $ do
        toUpdateEntityM <- I.getLastUpdated worker entity offset
        case toUpdateEntityM of
          Just (name, ts) ->
            pure
              . CrawlerPB.CommitInfoResponse
              . Just
              . CrawlerPB.CommitInfoResponseResultEntity
              . CrawlerPB.CommitInfoResponse_OldestEntity (Just $ fromEntityType entity (from name))
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

-- | /suggestions endpoint
searchSuggestions :: AuthResult AuthenticatedUser -> SearchPB.SuggestionsRequest -> AppM SearchPB.SuggestionsResponse
searchSuggestions auth request = checkAuth auth . const $ do
  GetTenants tenants <- getConfig
  let SearchPB.SuggestionsRequest {..} = request

  let tenantM = Config.lookupTenant tenants (from suggestionsRequestIndex)

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
  GetTenants tenants <- getConfig
  now <- getCurrentTime
  let requestE =
        do
          tenant <-
            Config.lookupTenant tenants (from tenantName)
              `orDie` ParseError "unknown tenant" 0

          expr <- P.parse (Q.loadAliases' tenant) (from queryText)

          query <-
            Q.queryWithMods now (from username) tenant expr

          pure (tenant, query)

  pure requestE

-- | /search/author endpoint
searchAuthor :: AuthResult AuthenticatedUser -> SearchPB.AuthorRequest -> AppM SearchPB.AuthorResponse
searchAuthor auth request = checkAuth auth . const $ do
  let SearchPB.AuthorRequest {..} = request
  GetTenants tenants <- getConfig
  let indexM = Config.lookupTenant tenants $ from authorRequestIndex

  authors <- case indexM of
    Just index -> do
      let toSearchAuthor muid = case Config.lookupIdent index muid of
            Nothing -> SearchPB.Author (from muid) mempty mempty
            Just Config.Ident {..} ->
              let authorMuid = from muid
                  authorAliases = V.fromList $ from <$> aliases
                  authorGroups = V.fromList $ from <$> fromMaybe mempty groups
               in SearchPB.Author {..}
      found <- runEmptyQueryM index $ I.searchAuthorCache . from $ authorRequestQuery
      pure $ toSearchAuthor <$> found
    Nothing -> pure []

  pure . SearchPB.AuthorResponse $ V.fromList authors

getMuidByIndexName :: Text -> AuthenticatedUser -> Maybe Text
getMuidByIndexName index = Map.lookup index . aMuidMap

-- | /search/check endpoint
searchCheck :: AuthResult AuthenticatedUser -> SearchPB.CheckRequest -> AppM SearchPB.CheckResponse
searchCheck auth request = checkAuth auth response
 where
  response authenticatedUserM = do
    let SearchPB.CheckRequest {..} = request
        username = from $ case authenticatedUserM of
          Just au -> fromMaybe (aDefaultMuid au) $ getMuidByIndexName (from checkRequestIndex) au
          Nothing -> from checkRequestUsername

    incCounter monocleQueryCheckCounter
    requestE <- validateSearchRequest checkRequestIndex checkRequestQuery username

    pure $
      SearchPB.CheckResponse $
        Just $ case requestE of
          Right _ -> SearchPB.CheckResponseResultSuccess "ok"
          Left (ParseError msg offset) ->
            SearchPB.CheckResponseResultError $
              SearchPB.QueryError (from msg) (fromInteger . toInteger $ offset)

-- | /search/query endpoint
searchQuery :: AuthResult AuthenticatedUser -> SearchPB.QueryRequest -> AppM SearchPB.QueryResponse
searchQuery auth request = checkAuth auth response
 where
  response authenticatedUserM = do
    let SearchPB.QueryRequest {..} = request
        username = from $ case authenticatedUserM of
          Just au -> fromMaybe (aDefaultMuid au) $ getMuidByIndexName (from queryRequestIndex) au
          Nothing -> from queryRequestUsername

    incCounter monocleQueryCounter
    requestE <- validateSearchRequest queryRequestIndex queryRequestQuery username

    case requestE of
      Right (tenant, query) -> runQueryM tenant (Q.ensureMinBound query) $ do
        let queryType = fromPBEnum queryRequestQueryType
        logEvent $ Searching queryType queryRequestQuery query

        case queryType of
          SearchPB.QueryRequest_QueryTypeQUERY_CHANGE ->
            SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultChanges
              . SearchPB.Changes
              . V.fromList
              . map from
              <$> Q.changes queryRequestOrder queryRequestLimit
          SearchPB.QueryRequest_QueryTypeQUERY_CHANGE_AND_EVENTS ->
            SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultChangeEvents
              . toChangeEventsResult
              <$> Q.changeEvents queryRequestChangeId queryRequestLimit
          SearchPB.QueryRequest_QueryTypeQUERY_REPOS_SUMMARY ->
            SearchPB.QueryResponse
              . Just
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
          SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_CREATED -> do
            top <- fromJust <$> Q.runMetricTop Q.metricChangeAuthors queryRequestLimit
            pure
              . SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultTopAuthors
              $ Q.toTermsCountPBInt top
          -- handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeCreated
          SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_CHANGES_MERGED ->
            handleTopAuthorsQ queryRequestLimit Q.getMostActiveAuthorByChangeMerged
          SearchPB.QueryRequest_QueryTypeQUERY_TOP_REVIEWED_AUTHORS ->
            handleTopAuthorsQ queryRequestLimit Q.getMostReviewedAuthor
          SearchPB.QueryRequest_QueryTypeQUERY_TOP_COMMENTED_AUTHORS ->
            handleTopAuthorsQ queryRequestLimit Q.getMostCommentedAuthor
          SearchPB.QueryRequest_QueryTypeQUERY_TOP_AUTHORS_PEERS ->
            SearchPB.QueryResponse
              . Just
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
            SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultChangesTops
              <$> Q.getChangesTops queryRequestLimit
          SearchPB.QueryRequest_QueryTypeQUERY_RATIO_COMMITS_VS_REVIEWS -> do
            ratio <- Q.getRatio Q.COMMITS_VS_REVIEWS_RATIO
            pure . SearchPB.QueryResponse . Just $
              SearchPB.QueryResponseResultRatio ratio
          SearchPB.QueryRequest_QueryTypeQUERY_HISTO_COMMITS -> do
            histo <- Q.runMetricTrendIntPB Q.metricChangeUpdates
            pure
              . SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultHisto
              $ MetricPB.HistoIntStat histo
          SearchPB.QueryRequest_QueryTypeQUERY_HISTO_REVIEWS_AND_COMMENTS -> do
            histo <- Q.runMetricTrendIntPB Q.metricReviewsAndComments
            pure
              . SearchPB.QueryResponse
              . Just
              . SearchPB.QueryResponseResultHisto
              $ MetricPB.HistoIntStat histo
      Left err -> pure . handleError $ err

  handleError :: ParseError -> SearchPB.QueryResponse
  handleError (ParseError msg offset) =
    SearchPB.QueryResponse
      . Just
      . SearchPB.QueryResponseResultError
      $ SearchPB.QueryError
        (from msg)
        (fromInteger . toInteger $ offset)

  handleTopAuthorsQ :: Word32 -> (Word32 -> QueryM Q.TermsResultWTH) -> QueryM SearchPB.QueryResponse
  handleTopAuthorsQ limit cb = do
    results <- cb limit
    pure
      . SearchPB.QueryResponse
      . Just
      . SearchPB.QueryResponseResultTopAuthors
      $ toTermsCount (V.fromList $ toTTResult <$> Q.tsrTR results) (toInt $ Q.tsrTH results)
   where
    toInt c = fromInteger $ toInteger c

  toAPeerResult :: Q.PeerStrengthResult -> SearchPB.AuthorPeer
  toAPeerResult Q.PeerStrengthResult {..} =
    SearchPB.AuthorPeer
      (from psrAuthor)
      (from psrPeer)
      psrStrength

  toTermsCount :: V.Vector MetricPB.TermCountInt -> Word32 -> MetricPB.TermsCountInt
  toTermsCount tcV total =
    let termsCountIntTermcount = tcV
        termsCountIntTotalHits = total
     in MetricPB.TermsCountInt {..}

  toTTResult :: Q.TermResult -> MetricPB.TermCountInt
  toTTResult Q.TermResult {..} =
    MetricPB.TermCountInt
      (from trTerm)
      (fromInteger $ toInteger trCount)

  toRSumResult :: Q.RepoSummary -> SearchPB.RepoSummary
  toRSumResult Q.RepoSummary {..} =
    let repoSummaryFullname = from fullname
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

searchFields :: AuthResult AuthenticatedUser -> SearchPB.FieldsRequest -> AppM SearchPB.FieldsResponse
searchFields auth _request = checkAuth auth . const $ response
 where
  response = pure . SearchPB.FieldsResponse . V.fromList . map toResult $ Q.fields
  toResult (name, (fieldType', _realname, desc)) =
    let fieldName = from name
        fieldDescription = from desc
        fieldType = Enumerated . Right $ fieldType'
     in SearchPB.Field {..}

metricList :: AuthResult AuthenticatedUser -> MetricPB.ListRequest -> AppM MetricPB.ListResponse
metricList auth _request = checkAuth auth . const $ response
 where
  response = pure . MetricPB.ListResponse . fromList . fmap toResp $ Q.allMetrics
  toResp Q.MetricInfo {..} =
    MetricPB.MetricInfo
      { metricInfoName = from miName
      , metricInfoDescription = from miDesc
      , metricInfoLongDescription = mempty
      , metricInfoMetric = from miMetricName
      }

class Num a => NumPB a where
  toNumResult :: Q.Numeric a -> MetricPB.GetResponse

instance NumPB Float where
  toNumResult (Q.Num v) = MetricPB.GetResponse . Just $ MetricPB.GetResponseResultFloatValue v

instance NumPB Word32 where
  toNumResult (Q.Num v) = MetricPB.GetResponse . Just . MetricPB.GetResponseResultIntValue . fromInteger $ toInteger v

class Num a => TrendPB a where
  toTrendResult :: V.Vector (Q.Histo a) -> MetricPB.GetResponse

instance TrendPB Float where
  toTrendResult v =
    MetricPB.GetResponse
      . Just
      . MetricPB.GetResponseResultHistoFloatValue
      . MetricPB.HistoFloatStat
      $ Q.toPBHistoFloat <$> v

instance TrendPB Word32 where
  toTrendResult v =
    MetricPB.GetResponse
      . Just
      . MetricPB.GetResponseResultHistoIntValue
      . MetricPB.HistoIntStat
      $ Q.toPBHistoInt <$> v

class Num a => TopPB a where
  toTopResult :: Maybe (Q.TermsCount a) -> MetricPB.GetResponse

toTopResultNotSupported :: MetricPB.GetResponse
toTopResultNotSupported =
  MetricPB.GetResponse
    . Just
    $ MetricPB.GetResponseResultError "This metric does not support Top option"

instance TopPB Word32 where
  toTopResult (Just v) =
    MetricPB.GetResponse
      . Just
      . MetricPB.GetResponseResultTopIntValue
      $ Q.toPBTermsCountInt v
  toTopResult Nothing = toTopResultNotSupported

instance TopPB Float where
  toTopResult (Just v) =
    MetricPB.GetResponse
      . Just
      . MetricPB.GetResponseResultTopFloatValue
      $ Q.toPBTermsCountFloat v
  toTopResult Nothing = toTopResultNotSupported

metricGet :: AuthResult AuthenticatedUser -> MetricPB.GetRequest -> AppM MetricPB.GetResponse
metricGet auth request = checkAuth auth response
 where
  handleError = pure . MetricPB.GetResponse . Just . MetricPB.GetResponseResultError
  response authenticatedUserM = do
    let MetricPB.GetRequest {..} = request
        username = from $ case authenticatedUserM of
          Just au -> fromMaybe (aDefaultMuid au) $ getMuidByIndexName (from getRequestIndex) au
          Nothing -> from getRequestUsername
    incCounter monocleMetricCounter
    requestE <- validateSearchRequest getRequestIndex getRequestQuery username
    case requestE of
      -- Valid request
      Right (tenant, query) -> runMetricQuery tenant query (from getRequestMetric) getRequestOptions
      -- Invalid request
      Left err -> handleError $ show err
  runMetricQuery ::
    Config.Index ->
    Q.Query ->
    Text ->
    Maybe MetricPB.GetRequestOptions ->
    AppM MetricPB.GetResponse
  runMetricQuery tenant query getRequestMetric getRequestOptions = do
    case getRequestMetric of
      "changes_created" -> runMetric Q.metricChangesCreated
      "changes_merged" -> runMetric Q.metricChangesMerged
      "changes_abandoned" -> runMetric Q.metricChangesAbandoned
      "change_updates" -> runMetric Q.metricChangeUpdates
      "change_with_tests_count" -> runMetric Q.metricChangeWithTests
      "changes_self_merged_count" -> runMetric Q.metricChangesSelfMerged
      "reviews" -> runMetric Q.metricReviews
      "comments" -> runMetric Q.metricComments
      "reviews_and_comments" -> runMetric Q.metricReviewsAndComments
      "review_authors" -> runMetric Q.metricReviewAuthors
      "comment_authors" -> runMetric Q.metricCommentAuthors
      "change_authors" -> runMetric Q.metricChangeAuthors
      "time_to_merge" -> runMetric Q.metricTimeToMerge
      "time_to_merge_variance" -> runMetric Q.metricTimeToMergeVariance
      "first_review_mean_time" -> runMetric Q.metricFirstReviewMeanTime
      "first_comment_mean_time" -> runMetric Q.metricFirstCommentMeanTime
      "commits_per_change" -> runMetric Q.metricCommitsPerChange
      -- Unknown query
      _ -> handleError $ "Unknown metric: " <> from getRequestMetric
   where
    runM = runQueryM tenant (Q.ensureMinBound query)
    runMetric m = case getRequestOptions of
      Just (MetricPB.GetRequestOptionsTrend (MetricPB.Trend interval)) ->
        toTrendResult <$> runM (Q.runMetricTrend m $ Just $ fromPBTrendInterval $ from interval)
      Just (MetricPB.GetRequestOptionsTop (MetricPB.Top limit)) -> toTopResult <$> runM (Q.runMetricTop m limit)
      _ -> toNumResult <$> runM (Q.runMetric m)
    fromPBTrendInterval :: Text -> Q.TimeRange
    fromPBTrendInterval interval = case readMaybe (from interval) of
      Just timeRange -> timeRange
      Nothing -> error "Unable to parse trend interval"

-- | gen a 302 redirect helper
redirects :: ByteString -> AppM ()
redirects url = throwError err302 {errHeaders = [("Location", url)]}

data Err = Err
  { errTitle :: Text
  , errMsg :: Text
  }

instance ToMarkup Err where
  toMarkup Err {..} = H.docTypeHtml $ do
    H.head $ do
      H.title "Error"
    H.body $ do
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

format :: ToMarkup a => a -> LBS.ByteString
format err = toMarkup err & renderMarkup

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

appToErr :: ServerError -> Text -> ServerError
appToErr x msg =
  x
    { errBody = from $ format (Err (from (errReasonPhrase x)) msg)
    , errHeaders = [("Content-Type", "text/html")]
    }

handleLogin :: Maybe Text -> AppM NoContent
handleLogin uriM = do
  aOIDC <- asks aOIDC
  case oidcEnv aOIDC of
    Just oidcenv -> do
      incCounter monocleAuthProviderRedirectCounter
      loc <- liftIO (genOIDCURL oidcenv)
      redirects loc
      pure NoContent
    Nothing -> forbidden "No OIDC Context"
 where
  genOIDCURL :: OIDCEnv -> IO ByteString
  genOIDCURL oidcenv@OIDCEnv {oidc} = do
    loc <- O.prepareAuthenticationRequestUrl (mkSessionStore oidcenv Nothing uriM) oidc [O.openId] mempty
    return (show loc)

handleLoggedIn ::
  -- | error
  Maybe Text ->
  -- | code
  Maybe Text ->
  -- | state
  Maybe Text ->
  AppM LoginInUser
handleLoggedIn err codeM stateM = do
  aOIDC <- asks aOIDC
  GetConfig config <- getConfig
  log $ OIDCCallbackCall err codeM stateM
  case (oidcEnv aOIDC, err, codeM, stateM) of
    (_, Just errorMsg, _, _) -> forbidden $ "Error from remote provider: " <> errorMsg
    (_, _, Nothing, _) -> forbidden "No code parameter given"
    (_, _, _, Nothing) -> forbidden "No state parameter given"
    (Nothing, _, _, _) -> forbidden "No OIDC Context"
    (Just oidcEnv, _, Just oauthCode, Just oauthState) -> do
      tokens :: O.Tokens Value <-
        liftIO $
          O.getValidTokens
            (mkSessionStore oidcEnv (Just $ from oauthState) Nothing)
            (oidc oidcEnv)
            (manager oidcEnv)
            (from oauthState)
            (from oauthCode)
      now <- liftIO Monocle.Prelude.getCurrentTime
      let idToken = O.idToken tokens
          expiry = addUTCTime (24 * 3600) now
          userId = aUserId oidcEnv idToken
          mUidMap = getIdents config $ "AuthProviderUID:" <> userId
      log . OIDCProviderTokenRequested $ show idToken
      jwtE <- liftIO $ mkJwt (localJWTSettings aOIDC) mUidMap userId (Just expiry)
      case jwtE of
        Right jwt -> do
          incCounter monocleAuthSuccessCounter
          log $ JWTCreated (show mUidMap) (decodeUtf8 $ redirectUri oidcEnv)
          let liJWT = decodeUtf8 jwt
              liRedirectURI = case decodeOIDCState $ encodeUtf8 oauthState of
                Just (OIDCState _ (Just uri)) -> uri
                _ -> "/"
          pure $ LoginInUser {..}
        Left err' -> do
          log $ JWTCreateFailed (show mUidMap) (show err')
          forbidden $ "Unable to generate user JWT due to: " <> show err'
 where
  -- Get the Token's claim that identify an unique user
  aUserId :: OIDCEnv -> O.IdTokenClaims Value -> Text
  aUserId OIDCEnv {providerConfig} idToken = case opUserClaim providerConfig of
    Just uc -> case O.otherClaims idToken of
      Object o -> case AKM.lookup (AK.fromText uc) o of
        Just (String s) -> s
        _ -> defaultUserId
      _ -> defaultUserId
    Nothing -> defaultUserId
   where
    defaultUserId = sub idToken
  log ev = do
    aEnv <- asks aEnv
    liftIO $ doLog (glLogger aEnv) $ via @Text $ ev
  -- Given a Claim, get a mapping of index (workspace) name to Monocle UID (mUid)
  getIdents :: Config -> Text -> Map.Map Text Text
  getIdents config auid = foldr go Map.empty $ Config.getWorkspaces config
   where
    go index acc = case Config.getIdentByAlias index auid of
      Just muid -> Map.insert (Config.getWorkspaceName index) muid acc
      Nothing -> acc
