{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Monocle.Api.Config
-- Description : Handle the Monocle configuration file
-- Copyright   : Monocle authors
-- License     : AGPL-3
--
-- The module contains defintion of data types according to the
-- Monocle dhall schemas found in the schemas/monocle/config directory. It also
-- provides some functions to handle configuration data.
module Monocle.Config (
  -- * Data types imported from dhall
  Config (..),
  Workspace,
  Index (..),
  IndexName,
  Project (..),
  Ident (..),
  SearchAlias (..),
  Crawlers (..),
  Crawler (..),
  Provider (..),
  Gitlab (..),
  Gerrit (..),
  Github (..),
  GithubUser (..),
  Bugzilla (..),
  GithubApplication (..),
  Link (..),
  OIDCProviderConfig (..),
  Auth (..),
  AuthProvider (..),
  OIDC (..),
  GithubAuth (..),

  -- * Data types to host the config status
  WorkspaceStatus,
  ConfigStatus (..),
  Status (..),

  -- * Functions related to config loading
  loadConfig,
  loadConfigWithoutEnv,
  reloadConfig,
  mkWorkspaceStatus,
  setWorkspaceStatus,

  -- * The Config Monad
  getSecret,

  -- * Functions to handle a Config
  getWorkspaces,
  getAuthProvider,

  -- * Functions to handle an Index
  getWorkspaceName,
  lookupTenant,
  lookupProject,
  lookupCrawler,
  lookupIdent,
  lookupGroupMembers,
  getTenantGroups,
  getTenantProjectsNames,
  getSearchAliases,
  getIdentByAlias,

  -- * Functions to handle a Crawler
  getPrefix,
  getCrawlerName,
  getCrawlerProject,
  getCrawlerProjectIssue,
  getCrawlerOrganization,
  getCrawlerTaskData,
  getCrawlerUser,

  -- * Some utility functions
  mkTenant,
  getIdentByAliasFromIdents,
  links,
  getIndexName,
  mkIndexName,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Char (isLetter, isLowerCase)
import Data.Either.Validation (Validation (Failure, Success))
import Data.Map qualified as Map
import Data.Text qualified as T (all, dropWhileEnd, isPrefixOf, null, replace, toUpper, uncons, unpack)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Dhall qualified
import Dhall.Core qualified
import Dhall.YamlToDhall qualified as Dhall
import Effectful.Env
import Monocle.Config.Generated
import Monocle.Prelude
import Servant.API (FromHttpApiData(..))
import System.Directory (getModificationTime)
import Witch qualified

data Config = Config
  { about :: Maybe About
  , auth :: Maybe Auth
  , crawlers :: Maybe Crawlers
  , workspaces :: [Workspace]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

-- | Workspace are not index name.
type Workspace = Index

data Index = Index
  { name :: IndexName
  , crawlers_api_key :: Maybe Text
  , crawlers :: [Crawler]
  , projects :: Maybe [Project]
  , idents :: Maybe [Ident]
  , search_aliases :: Maybe [SearchAlias]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

newtype IndexName
  = IndexName { getIndexName :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Dhall.ToDhall, Aeson.ToJSON, Aeson.ToJSONKey, Aeson.FromJSONKey)

instance Aeson.FromJSON IndexName where
  parseJSON x =
    parseJSON x >>= either (fail . T.unpack) return . mkIndexName

instance Dhall.FromDhall IndexName where
  autoWith _ = Dhall.Decoder {..}
    where
      expected = pure Dhall.Core.Text
      extract =
        \case
          Dhall.Core.TextLit (Dhall.Core.Chunks [] t) -> either Dhall.extractError pure $ mkIndexName t
          expr                                        -> Dhall.typeError expected expr

deriving anyclass instance Witch.From Text IndexName

deriving instance Witch.From IndexName Text

instance Witch.From TL.Text IndexName where
  from = IndexName . TL.toStrict

instance Witch.From IndexName TL.Text where
  from = TL.fromStrict . getIndexName

instance FromHttpApiData IndexName where
  parseUrlPiece = mkIndexName

-- End - Loading of Types from the dhall-monocle

-- Begin - Configuration loading system
---------------------------------------

-- | Load the YAML config file
loadConfigWithoutEnv :: MonadIO m => FilePath -> m Config
loadConfigWithoutEnv configPath = do
  -- Here we use the yaml-to-dhall logic to correctly decode Union value.
  -- Otherwise the decoder may fail with:
  -- AesonException "Error in $.tenants[1].crawlers[0].provider: parsing "
  --   Monocle.Api.Config.Provider(GitlabProvider) failed, key \"contents\" not found"
  --
  -- dhallFromYaml is able to infer the sum type by its value and it picks
  -- the first constructor that fit.
  expr <- liftIO $ Dhall.dhallFromYaml loadOpt =<< BS.readFile configPath
  pure $ case Dhall.extract Dhall.auto expr of
    Success config' -> config'
    Failure err -> error $ "Invalid configuration: " <> show err
 where
  configType = Dhall.Core.pretty configurationSchema
  loadOpt = Dhall.defaultOptions $ Just configType

-- | Load the YAML config file and resolv environment variables
loadConfig :: MonadIO m => FilePath -> m Config
loadConfig configPath = do
  config <- loadConfigWithoutEnv configPath
  configWorkspaces <- traverse resolveEnv $ workspaces config
  pure $ config {workspaces = configWorkspaces}
 where
  crawlersApiKeyLens :: Lens' Index Text
  crawlersApiKeyLens =
    lens
      (fromMaybe "CRAWLERS_API_KEY" . crawlers_api_key)
      (\index newKey -> index {crawlers_api_key = Just newKey})
  resolveEnv :: MonadIO m => Index -> m Index
  resolveEnv = liftIO . mapMOf crawlersApiKeyLens getEnv'

-- | A Type to express if a 'Workspace' needs refresh
data Status = NeedRefresh | Ready

type WorkspaceName = IndexName

type WorkspaceStatus = Map WorkspaceName Status

-- | The 'ConfigStatus' wraps the loaded Monocle config
data ConfigStatus = ConfigStatus
  { csReloaded :: Bool
  -- ^ Is the config has been reloaded from disk
  , csConfig :: Config
  -- ^ The 'Config'
  , csWorkspaceStatus :: MVar WorkspaceStatus
  -- ^ The refresh status of a Workspace
  }

-- | Return a 'WorkspaceStatus' with all workspace 'Status' set on 'NeedRefresh'
mkWorkspaceStatus :: Config -> WorkspaceStatus
mkWorkspaceStatus config = fromList $ mkStatus <$> getWorkspaces config
 where
  mkStatus ws = (getWorkspaceName ws, NeedRefresh)

-- | Set all workspaces 'Status' on a given 'Status'
setWorkspaceStatus :: Status -> MVar WorkspaceStatus -> IO ()
setWorkspaceStatus status wsRef = modifyMVar_ wsRef $ pure . fmap (const status)

-- | An IO action that reload the config if needed. It does by checking last
-- modification time from disk then provides the config wrapped in a 'ConfigStatus'.
reloadConfig :: FilePath -> IO (IO ConfigStatus)
reloadConfig fp = do
  -- Get the current config
  configTS <- getModificationTime fp
  config <- loadConfig fp

  -- Create the reload action
  tsRef <- newMVar (configTS, config)
  wsRef <- newMVar $ mkWorkspaceStatus config
  pure (modifyMVar tsRef (reload wsRef))
 where
  reload wsRef mvar@(prevConfigTS, prevConfig) = do
    configTS <- getModificationTime fp
    if configTS > prevConfigTS
      then do
        -- TODO: use log reload event
        putTextLn $ from fp <> ": reloading config"
        config <- loadConfig fp
        modifyMVar_ wsRef (const . pure $ mkWorkspaceStatus config)
        pure ((configTS, config), ConfigStatus True config wsRef)
      else pure (mvar, ConfigStatus False prevConfig wsRef)

-- | Return a 'Secret' based on environment variable
getSecret ::
  EnvEffect :> es =>
  -- | Default environment key name
  Text ->
  -- | The environment key name
  Maybe Text ->
  Eff es Secret
getSecret def keyM =
  from
    . fromMaybe (error $ "Missing environment: " <> env)
    <$> envGet (unTagged $ into @(UTF_8 ByteString) env)
 where
  env = fromMaybe def keyM

data OIDCProviderConfig = OIDCProviderConfig
  { opIssuerURL :: Text
  , opClientID :: Text
  , opClientSecret :: Text
  , opAppPublicURL :: Text
  , opUserClaim :: Maybe Text
  , opEnforceAuth :: Bool
  , opName :: Text
  }

-- End - Configuration loading system

-- Begin - Functions to handle a Config
---------------------------------------

-- | Simply returns the config workspaces
getWorkspaces :: Config -> [Index]
getWorkspaces Config {..} = workspaces

-- | Get Authentication provider from config
getAuthProvider :: Text -> Config -> IO (Either Text (Maybe OIDCProviderConfig))
getAuthProvider publicUrl Config {auth} = case auth of
  Just Auth {..} -> do
    case auth_provider of
      OIDCProvider (OIDC {..}) | (not . any T.null) [oidc_issuer_url, oidc_client_id, oidc_provider_name] ->
        do
          let secEnv = secretEnv oidc_provider_name
          opClientSecretM <- fmap from <$> lookupEnv secEnv
          case opClientSecretM of
            Just opClientSecret | not (T.null opClientSecret) -> mkProvider
             where
              mkProvider =
                let opIssuerURL = ensureTrailingSlash oidc_issuer_url
                    opClientID = oidc_client_id
                    opUserClaim = oidc_user_claim
                    opName = oidc_provider_name
                    opEnforceAuth = Just True == enforce_auth
                 in pure . Right . Just $ OIDCProviderConfig {..}
            Just _ ->
              pure
                . Left
                $ misconfigMsg
                <> from secEnv
                <> " environment variable found must not be empty."
            Nothing ->
              pure
                . Left
                $ misconfigMsg
                <> from secEnv
                <> " environment variable not found."
      OIDCProvider _ -> do
        pure . Left $ misconfigMsg <> "At least one mandatory setting is empty."
      GithubAuthProvider _ -> error "GithubAuthProvider not yet supported"
  Nothing -> pure $ Right Nothing
 where
  opAppPublicURL = ensureTrailingSlash publicUrl
  ensureTrailingSlash iss = T.dropWhileEnd (== '/') iss <> "/"
  providerNameToEnvFragment = T.replace " " "_" . T.toUpper
  secretEnv pname = "MONOCLE_OIDC_" <> from (providerNameToEnvFragment pname) <> "_CLIENT_SECRET"
  misconfigMsg = "OIDCProvider is misconfigured. "

-- End - Functions to handle a Config

-- Begin - Functions to handle an Index
---------------------------------------

-- | Get the 'Index' name
getWorkspaceName :: Index -> IndexName
getWorkspaceName Index {..} = name

-- | Find an 'Index' by name
lookupTenant :: [Index] -> IndexName -> Maybe Index
lookupTenant xs tenantName = find isTenant xs
 where
  isTenant Index {..} = name == tenantName

-- | Find a 'Project' in an 'Index'
lookupProject :: Index -> Text -> Maybe Project
lookupProject index projectName = find isProject (fromMaybe [] (projects index))
 where
  isProject :: Project -> Bool
  isProject Project {..} = name == projectName

-- | Find a 'Crawler' in an 'Index'
lookupCrawler :: Index -> Text -> Maybe Crawler
lookupCrawler index crawlerName = find isCrawler index.crawlers
 where
  isCrawler Crawler {..} = name == crawlerName

-- | Find an 'Ident' in an 'Index'
lookupIdent :: Index -> Text -> Maybe Ident
lookupIdent Index {..} userName = find isUser (fromMaybe [] idents)
 where
  isUser Ident {..} = ident == userName

-- | Find groups members of a group in an 'Index'
lookupGroupMembers :: Index -> Text -> Maybe (NonEmpty Text)
lookupGroupMembers Index {..} groupName = case foldr go [] (fromMaybe [] idents) of
  [] -> Nothing
  (x : xs) -> Just (x :| xs)
 where
  -- For each ident, check if it is a member of groupName.
  -- If it is, then add the ident name to the list
  go :: Ident -> [Text] -> [Text]
  go Ident {..} acc = case groups of
    Just xs
      | groupName `elem` xs -> ident : acc
      | otherwise -> acc
    Nothing -> acc

-- | Get the list of group and members
getTenantGroups :: Index -> [(Text, [Text])]
getTenantGroups index = Map.toList $ foldr go mempty (fromMaybe [] (idents index))
 where
  go :: Ident -> Map Text [Text] -> Map Text [Text]
  go Ident {..} acc = foldr (addUser ident) acc (fromMaybe [] groups)
  addUser :: Text -> Text -> Map Text [Text] -> Map Text [Text]
  addUser name groupName acc =
    let users' = fromMaybe [] (Map.lookup groupName acc)
     in Map.insert groupName (users' <> [name]) acc

-- | Get the list of projects
getTenantProjectsNames :: Index -> [Text]
getTenantProjectsNames index = maybe [] (map getName) (projects index)
 where
  getName Project {..} = name

-- | Find search aliases in an 'Index'
getSearchAliases :: Index -> [(Text, Text)]
getSearchAliases index = maybe [] (fmap toTuple) (search_aliases index)
 where
  toTuple SearchAlias {..} = (name, alias)

-- | Get the Ident name for a Given alias
getIdentByAlias :: Index -> Text -> Maybe Text
getIdentByAlias Index {..} alias = getIdentByAliasFromIdents alias =<< idents

-- End - Functions to handle an Index

-- Begin - Functions to handle a Crawler
----------------------------------------

-- | Get the 'Crawler' name
getCrawlerName :: Crawler -> Text
getCrawlerName Crawler {..} = name

-- | Get the 'Crawler' prefix (to be prepend to repository name)
getPrefix :: Crawler -> Maybe Text
getPrefix Crawler {..} = case provider of
  GerritProvider Gerrit {..} -> gerrit_prefix
  _ -> Nothing

-- | Get 'Crawler' project names
getCrawlerProject :: Crawler -> [Text]
getCrawlerProject Crawler {..} = case provider of
  GitlabProvider Gitlab {..} ->
    let addOrgPrefix = getPath gitlab_organization
     in addOrgPrefix <$> fromMaybe [] gitlab_repositories
  GithubProvider Github {..} ->
    let addOrgPrefix = getPath github_organization
     in addOrgPrefix <$> fromMaybe [] github_repositories
  GerritProvider Gerrit {..} -> maybe [] (filter (not . T.isPrefixOf "^")) gerrit_repositories
  _anyOtherProvider -> []

-- | Get 'Crawler' user names
getCrawlerUser :: Crawler -> [Text]
getCrawlerUser Crawler {..} = case provider of
  GithubUserProvider GithubUser {..} -> github_users
  _anyOtherProvider -> []

-- | Get 'Crawler' organization names
getCrawlerOrganization :: Crawler -> [Text]
getCrawlerOrganization Crawler {..} = case provider of
  GitlabProvider Gitlab {..} -> [gitlab_organization]
  GithubProvider Github {..} -> [github_organization]
  GerritProvider Gerrit {..} -> maybe [] (filter (T.isPrefixOf "^")) gerrit_repositories
  _anyOtherProvider -> []

-- | Get 'Crawler' TaskData project names
getCrawlerTaskData :: Crawler -> [Text]
getCrawlerTaskData Crawler {..} = case provider of
  GithubProvider Github {..} ->
    let addOrgPrefix = getPath github_organization
     in addOrgPrefix <$> fromMaybe [] github_repositories
  BugzillaProvider Bugzilla {..} -> fromMaybe [] bugzilla_products
  _anyOtherProvider -> []

getCrawlerProjectIssue :: Crawler -> [Text]
getCrawlerProjectIssue crawler@Crawler {..} = case provider of
  GithubProvider Github {} -> getCrawlerTaskData crawler
  -- todo: add JiraProvider here
  _anyOtherProvider -> []

-- End - Functions to handle a Crawler

-- Begin - Some utility functions
---------------------------------

-- | Create an empty 'Index'
mkTenant :: IndexName -> Index
mkTenant name =
  Index
    { name
    , crawlers = []
    , crawlers_api_key = Nothing
    , projects = Nothing
    , idents = Nothing
    , search_aliases = Nothing
    }

-- | Get 'Ident' ident from a list of 'Ident'
getIdentByAliasFromIdents :: Text -> [Ident] -> Maybe Text
getIdentByAliasFromIdents alias idents' = case find isMatched idents' of
  Nothing -> Nothing
  Just Ident {..} -> Just ident
 where
  isMatched :: Ident -> Bool
  isMatched Ident {..} = alias `elem` aliases

mkIndexName :: Text -> Either Text IndexName
mkIndexName name = do
  let check explanation p = if p then Right () else Left explanation
  check "Is empty" $ not $ T.null name
  check "Is longer than 255 bytes" $ BS.length (T.encodeUtf8 name) < 256
  check "Contains uppercase letter(s)" $ T.all (\x -> not (isLetter x) || isLowerCase x) name
  check "Includes [\\/*?\"<>| ,#:]" $ T.all (flip @_ @String notElem "\\/*?\"<>| ,#:") name
  check "Starts with [-_+.]" $ maybe False (flip @_ @String notElem "-_+." . fst) $ T.uncons name
  check "Is (.|..)" $ notElem name [".", ".."]
  return $ IndexName name

-- End - Some utility functions
