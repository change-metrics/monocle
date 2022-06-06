-- | The servant endpoint implementation.
-- This module provides an interface between the backend and the frontend
module Monocle.Api.Server where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Google.Protobuf.Timestamp as Timestamp
import Google.Protobuf.Timestamp qualified as T
import Monocle.Api.Jwt
  ( AuthenticatedUser (AUser),
    LoginInUser (..),
    OIDCEnv (..),
    mkJwt,
    mkSessionStore,
  )
import Monocle.Backend.Documents
  ( EChange (..),
    EChangeEvent (..),
  )
import Monocle.Backend.Index as I
import Monocle.Backend.Queries qualified as Q
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Prelude
import Monocle.Protob.Auth qualified as AuthPB
import Monocle.Protob.Config qualified as ConfigPB
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Login qualified as LoginPB
import Monocle.Protob.Metric qualified as MetricPB
import Monocle.Protob.Search qualified as SearchPB
import Monocle.Search.Parser qualified as P
import Monocle.Search.Query qualified as Q
import Monocle.Search.Syntax (ParseError (..))
import Monocle.Version (version)
import Proto3.Suite (Enumerated (..))
import Servant
  ( NoContent (..),
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

-- curl -XPOST -d '{"void": ""}' -H "Content-type: application/json" -H 'Authorization: Bearer <token>' http://localhost:8080/auth/whoami
authWhoAmi :: AuthResult AuthenticatedUser -> AuthPB.WhoAmiRequest -> AppM AuthPB.WhoAmiResponse
authWhoAmi (Authenticated (AUser muid _groups _aliases)) _request = response
  where
    response =
      pure $
        AuthPB.WhoAmiResponse
          . Just
          . AuthPB.WhoAmiResponseResultUid
          $ from muid
authWhoAmi _auth _request =
  pure $
    AuthPB.WhoAmiResponse
      . Just
      . AuthPB.WhoAmiResponseResultError
      . Enumerated
      $ Right AuthPB.WhoAmiErrorUnAuthorized

-- curl -XPOST -d '{"token": "admin-token"}' -H "Content-type: application/json" http://localhost:8080/auth/get
authGetMagicJwt :: AuthResult AuthenticatedUser -> AuthPB.GetMagicJwtRequest -> AppM AuthPB.GetMagicJwtResponse
authGetMagicJwt _auth (AuthPB.GetMagicJwtRequest inputAdminToken) = do
  oidc <- asks aOIDC
  -- The generated JWT does not have any expiry
  -- An API restart generates new JWK that will invalidate the token
  jwtE <- liftIO $ mkJwt (localJWTSettings oidc) "bot" Nothing
  adminTokenM <- liftIO $ lookupEnv "ADMIN_TOKEN"
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
      GetConfig config <- getConfig
      let aboutVersion = from version
          links = maybe [] Config.links (Config.about config)
          aboutLinks = fromList $ toLink <$> links
          authProvider = Config.getAuthProvider config
          aboutAuthentication = toAboutAuthentication <$> authProvider
      pure $ ConfigPB.GetAboutResponse $ Just ConfigPB.About {..}
    toLink :: Config.Link -> ConfigPB.About_AboutLink
    toLink Config.Link {..} =
      let about_AboutLinkName = from name
          about_AboutLinkUrl = from url
          about_AboutLinkCategory = from $ fromMaybe "About" category
       in ConfigPB.About_AboutLink {..}
    toAboutAuthentication Config.OIDCProvider {..} =
      let authConfigIssuer = from issuer
          authConfigClientId = from client_id
          authConfigUserClaim = from user_claim
       in ConfigPB.AboutAuthenticationConfig $ ConfigPB.AuthConfig {..}

-- | /api/2/get_workspaces endpoint
configGetWorkspaces :: AuthResult AuthenticatedUser -> ConfigPB.GetWorkspacesRequest -> AppM ConfigPB.GetWorkspacesResponse
configGetWorkspaces _auth _request = response
  where
    response = do
      GetTenants tenants <- getConfig
      pure . ConfigPB.GetWorkspacesResponse . V.fromList $ map toWorkspace tenants
    toWorkspace Config.Index {..} =
      let workspaceName = from name
       in ConfigPB.Workspace {..}

-- | /api/2/get_groups endpoint
configGetGroups :: AuthResult AuthenticatedUser -> ConfigPB.GetGroupsRequest -> AppM ConfigPB.GetGroupsResponse
configGetGroups _auth request = do
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
configGetGroupMembers _auth request = do
  GetTenants tenants <- getConfig
  let ConfigPB.GetGroupMembersRequest {..} = request
  members <- case Config.lookupTenant tenants (from getGroupMembersRequestIndex) of
    Just index -> pure $ fromMaybe [] $ lookup (from getGroupMembersRequestGroup) (Config.getTenantGroups index)
    Nothing -> pure []

  pure . ConfigPB.GetGroupMembersResponse . V.fromList $ from <$> members

-- | /api/2/get_projects
configGetProjects :: AuthResult AuthenticatedUser -> ConfigPB.GetProjectsRequest -> AppM ConfigPB.GetProjectsResponse
configGetProjects _auth ConfigPB.GetProjectsRequest {..} = do
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
  GetTenants tenants <- getConfig
  pure $ do
    index <- Config.lookupTenant tenants (from indexName) `orDie` TDUnknownIndex
    crawler <- Config.lookupCrawler index (from crawlerName) `orDie` TDUnknownCrawler
    when (isJust apiKey) $
      when (Config.crawlers_api_key index /= (from <$> apiKey)) (Left TDUnknownApiKey)
    when checkCommitDate $
      void commitDate `orDie` TDDateInvalid
    pure (index, crawler, T.toUTCTime <$> commitDate)

-- | /suggestions endpoint
searchSuggestions :: AuthResult AuthenticatedUser -> SearchPB.SuggestionsRequest -> AppM SearchPB.SuggestionsResponse
searchSuggestions _auth request = do
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
searchAuthor _auth request = do
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

-- | /search/check endpoint
searchCheck :: AuthResult AuthenticatedUser -> SearchPB.CheckRequest -> AppM SearchPB.CheckResponse
searchCheck _auth request = do
  let SearchPB.CheckRequest {..} = request

  incCounter monocleQueryCheckCounter
  requestE <- validateSearchRequest checkRequestIndex checkRequestQuery checkRequestUsername

  pure $
    SearchPB.CheckResponse $
      Just $ case requestE of
        Right _ -> SearchPB.CheckResponseResultSuccess "ok"
        Left (ParseError msg offset) ->
          SearchPB.CheckResponseResultError $
            SearchPB.QueryError (from msg) (fromInteger . toInteger $ offset)

-- | /search/query endpoint
searchQuery :: AuthResult AuthenticatedUser -> SearchPB.QueryRequest -> AppM SearchPB.QueryResponse
searchQuery _auth request = do
  let SearchPB.QueryRequest {..} = request

  incCounter monocleQueryCounter
  requestE <- validateSearchRequest queryRequestIndex queryRequestQuery queryRequestUsername

  case requestE of
    Right (tenant, query) -> runQueryM tenant (Q.ensureMinBound query) $ do
      let queryType = fromPBEnum queryRequestQueryType
      logEvent $ Searching queryType queryRequestQuery query

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
        SearchPB.QueryRequest_QueryTypeQUERY_RATIO_COMMITS_VS_REVIEWS -> do
          ratio <- Q.getRatio Q.COMMITS_VS_REVIEWS_RATIO
          pure . SearchPB.QueryResponse . Just $
            SearchPB.QueryResponseResultRatio ratio
        SearchPB.QueryRequest_QueryTypeQUERY_HISTO_COMMITS -> do
          histo <- Q.getHisto Q.COMMITS_HISTO
          pure . SearchPB.QueryResponse . Just $
            SearchPB.QueryResponseResultHisto $ SearchPB.HistoStat histo
        SearchPB.QueryRequest_QueryTypeQUERY_HISTO_REVIEWS_AND_COMMENTS -> do
          histo <- Q.getHisto Q.REVIEWS_AND_COMMENTS_HISTO
          pure . SearchPB.QueryResponse . Just $
            SearchPB.QueryResponseResultHisto $ SearchPB.HistoStat histo
    Left err -> pure . handleError $ err
  where
    handleError :: ParseError -> SearchPB.QueryResponse
    handleError (ParseError msg offset) =
      SearchPB.QueryResponse . Just
        . SearchPB.QueryResponseResultError
        $ SearchPB.QueryError
          (from msg)
          (fromInteger . toInteger $ offset)

    handleTopAuthorsQ :: Word32 -> (Word32 -> QueryM Q.TermsResultWTH) -> QueryM SearchPB.QueryResponse
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
        (from psrAuthor)
        (from psrPeer)
        psrStrength

    toTermsCount :: V.Vector SearchPB.TermCount -> Word32 -> SearchPB.TermsCount
    toTermsCount tcV total =
      let termsCountTermcount = tcV
          termsCountTotalHits = total
       in SearchPB.TermsCount {..}

    toTTResult :: Q.TermResult -> SearchPB.TermCount
    toTTResult Q.TermResult {..} =
      SearchPB.TermCount
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
searchFields _auth _request = pure response
  where
    response :: SearchPB.FieldsResponse
    response = SearchPB.FieldsResponse . V.fromList . map toResult $ Q.fields
    toResult (name, (fieldType', _realname, desc)) =
      let fieldName = from name
          fieldDescription = from desc
          fieldType = Enumerated . Right $ fieldType'
       in SearchPB.Field {..}

lookupTenant :: Text -> AppM (Maybe Config.Index)
lookupTenant name = do
  GetTenants tenants <- getConfig
  pure $ Config.lookupTenant tenants name

metricList :: AuthResult AuthenticatedUser -> MetricPB.ListRequest -> AppM MetricPB.ListResponse
metricList _auth _request = pure . MetricPB.ListResponse . fromList . fmap toResp $ Q.allMetrics
  where
    toResp Q.MetricInfo {..} =
      MetricPB.MetricInfo
        { metricInfoName = from miName,
          metricInfoDescription = from miDesc,
          metricInfoLongDescription = mempty,
          metricInfoMetric = from miMetricName
        }

metricGet :: AuthResult AuthenticatedUser -> MetricPB.GetRequest -> AppM MetricPB.GetResponse
metricGet _auth request = do
  let MetricPB.GetRequest {..} = request
  incCounter monocleMetricCounter
  requestE <- validateSearchRequest getRequestIndex getRequestQuery getRequestUsername
  case requestE of
    -- Valid request
    Right (tenant, query) -> do
      let runMetric = runQueryM tenant (Q.ensureMinBound query)
          floatResult metric =
            MetricPB.GetResponse . Just . MetricPB.GetResponseResultFloatValue <$> runMetric (Q.runMetric metric)

      case getRequestMetric of
        "time_to_merge" -> floatResult Q.metricTimeToMerge
        -- Unknown query
        _ -> handleError $ "Unknown metric: " <> from getRequestMetric

    -- Invalid request
    Left err -> handleError $ show err
  where
    handleError = pure . MetricPB.GetResponse . Just . MetricPB.GetResponseResultError

-- | gen a 302 redirect helper
redirects :: ByteString -> AppM ()
redirects url = throwError err302 {errHeaders = [("Location", url)]}

data Err = Err
  { errTitle :: Text,
    errMsg :: Text
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
    { errBody = from $ format (Err (from (errReasonPhrase x)) msg),
      errHeaders = [("Content-Type", "text/html")]
    }

handleLogin :: AppM NoContent
handleLogin = do
  aOIDC <- asks aOIDC
  case oidcEnv aOIDC of
    Just oidcenv -> do
      loc <- liftIO (genOIDCURL oidcenv)
      redirects loc
      pure NoContent
    Nothing -> forbidden "No OIDC Context"
  where
    genOIDCURL :: OIDCEnv -> IO ByteString
    genOIDCURL oidcenv@OIDCEnv {oidc} = do
      loc <- O.prepareAuthenticationRequestUrl (mkSessionStore oidcenv Nothing) oidc [O.openId] mempty
      return (show loc)

handleLoggedIn ::
  -- | error
  Maybe Text ->
  -- | code
  Maybe Text ->
  -- | state
  Maybe Text ->
  AppM LoginInUser
handleLoggedIn err codeE stateE = do
  aOIDC <- asks aOIDC
  case (oidcEnv aOIDC, err, codeE, stateE) of
    (_, Just errorMsg, _, _) -> forbidden $ "Error from remote provider: " <> errorMsg
    (_, _, Nothing, _) -> forbidden "No code parameter given"
    (_, _, _, Nothing) -> forbidden "No state parameter given"
    (Nothing, _, _, _) -> forbidden "No OIDC Context"
    (Just oidcEnv, _, Just oauthCode, Just oauthState) -> do
      tokens :: O.Tokens Value <-
        liftIO $
          O.getValidTokens
            (mkSessionStore oidcEnv (Just $ from oauthState))
            (oidc oidcEnv)
            (manager oidcEnv)
            (from oauthState)
            (from oauthCode)
      let idToken = O.idToken tokens
          -- TODO will need to check here for extra claims
          _otherClaims = O.otherClaims idToken
      now <- liftIO $ Monocle.Prelude.getCurrentTime
      let expiry = addUTCTime (24 * 3600) now
      jwtE <- liftIO $ mkJwt (localJWTSettings aOIDC) (sub idToken) (Just expiry)
      case jwtE of
        Right jwt -> let liJWT = decodeUtf8 jwt in pure $ LoginInUser {..}
        Left err' -> forbidden $ "Unable to generate user JWT due to: " <> show err'
