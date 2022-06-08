{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Monocle.Api.Config
-- Description : Handle the Monocle configuration file
-- Copyright   : Monocle authors
-- License     : AGPL-3
--
-- The module contains defintion of data types according to the
-- dhall-monocle project (loaded as a git submodule). It also
-- provides some functions to handle configuration data.
module Monocle.Config
  ( -- * Data types imported from dhall (dhall-monocle)
    Config (..),
    Index (..),
    Project (..),
    Ident (..),
    SearchAlias (..),
    Crawler (..),
    Provider (..),
    Gitlab (..),
    Gerrit (..),
    Github (..),
    Bugzilla (..),
    GithubApplication (..),
    Link (..),
    OIDCProvider (..),

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
    MonadConfig (..),
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
    getCrawlerOrganization,
    getCrawlerTaskData,

    -- * Some utility functions
    mkTenant,
    getIdentByAliasFromIdents,
    links,
  )
where

import Data.ByteString qualified as BS
import Data.Either.Validation (Validation (Failure, Success))
import Data.Map qualified as Map
import Data.Text qualified as T (dropWhileEnd, isPrefixOf)
import Dhall qualified
import Dhall.Core qualified
import Dhall.Src qualified
import Dhall.TH qualified
import Dhall.YamlToDhall qualified as Dhall
import Monocle.Prelude
import System.Directory (getModificationTime)

-- Begin - Loading of Types from the dhall-monocle
--------------------------------------------------

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  ( let providerPath name = "./dhall-monocle/Monocle/Provider/" <> name <> "/Type.dhall"
        provider name = Dhall.TH.SingleConstructor name name $ providerPath name
        mainPath name = "./dhall-monocle/Monocle/" <> name <> "/Type.dhall"
        main name = Dhall.TH.SingleConstructor name name $ mainPath name
     in [ main "Project",
          main "Ident",
          main "SearchAlias",
          main "Crawler",
          main "Config",
          main "About",
          main "Link",
          provider "Gerrit",
          provider "Gitlab",
          provider "Github",
          provider "GithubApplication",
          provider "Bugzilla",
          Dhall.TH.MultipleConstructors
            "Provider"
            "./dhall-monocle/Monocle/Crawler/Provider.dhall",
          -- To support backward compatible schema, we replace Index and Crawler schemas
          Dhall.TH.SingleConstructor "Index" "Index" $ mainPath "Workspace"
        ]
  )

-- | Embed the expected configuration schema
configurationSchema :: Dhall.Core.Expr Dhall.Src.Src Void
configurationSchema = $(Dhall.TH.staticDhallExpression "./dhall-monocle/Monocle/Config/Type.dhall")

deriving instance Eq Gerrit

deriving instance Show Gerrit

deriving instance Eq Github

deriving instance Show Github

deriving instance Eq GithubApplication

deriving instance Show GithubApplication

deriving instance Eq Gitlab

deriving instance Show Gitlab

deriving instance Eq Bugzilla

deriving instance Show Bugzilla

deriving instance Eq Project

deriving instance Show Project

deriving instance Eq Provider

deriving instance Show Provider

deriving instance Eq Crawler

deriving instance Show Crawler

deriving instance Eq Ident

deriving instance Show Ident

deriving instance Eq SearchAlias

deriving instance Show SearchAlias

deriving instance Eq Index

deriving instance Show Index

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

type WorkspaceName = Text

type WorkspaceStatus = Map WorkspaceName Status

-- | The 'ConfigStatus' wraps the loaded Monocle config
data ConfigStatus = ConfigStatus
  { -- | Is the config has been reloaded from disk
    csReloaded :: Bool,
    -- | The 'Config'
    csConfig :: Config,
    -- | The refresh status of a Workspace
    csWorkspaceStatus :: MVar WorkspaceStatus
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
  MonadIO m =>
  -- | Default environment key name
  Text ->
  -- | The environment key name
  Maybe Text ->
  m Secret
getSecret def keyM =
  Secret . from . fromMaybe (error $ "Missing environment: " <> env)
    <$> lookupEnv (from env)
  where
    env = fromMaybe def keyM

class MonadConfig m where
  -- | Return a 'Secret' based on environment variable
  mGetSecret :: Text -> Maybe Text -> m Secret

  -- | An IO action that reload the config if needed
  mReloadConfig :: FilePath -> m (m ConfigStatus)

instance MonadConfig IO where
  mGetSecret = getSecret
  mReloadConfig = reloadConfig

data OIDCProvider = OIDCProvider
  { opIssuerURL :: Text,
    opClientID :: Text,
    opClientSecret :: Text,
    opAppPublicURL :: Text,
    opUserClaim :: Maybe Text,
    opEnforceAuth :: Bool
  }

-- | Get Authentication provider config from Env
getAuthProvider :: IO (Maybe OIDCProvider)
getAuthProvider = do
  opIssuerURL' <- lookupEnv "MONOCLE_OIDC_ISSUER_URL"
  opClientID' <- lookupEnv "MONOCLE_OIDC_CLIENT_ID"
  opClientSecret' <- lookupEnv "MONOCLE_OIDC_CLIENT_SECRET"
  opAppPublicURL' <- lookupEnv "MONOCLE_PUBLIC_URL"
  opUserClaim' <- lookupEnv "MONOCLE_OIDC_USER_CLAIM"
  opEnforceAuth' <- lookupEnv "MONOCLE_ENFORCE_AUTH"
  pure $ case (opIssuerURL', opClientID', opClientSecret', opAppPublicURL') of
    ( fmap (ensureTrailingSlash . from) -> Just opIssuerURL,
      fmap from -> Just opClientID,
      fmap from -> Just opClientSecret,
      fmap (ensureTrailingSlash . from) -> Just opAppPublicURL
      ) ->
        let opUserClaim = from <$> opUserClaim'
            opEnforceAuth = maybe False (not . null) opEnforceAuth'
         in Just $ OIDCProvider {..}
    _ -> Nothing
  where
    ensureTrailingSlash iss = T.dropWhileEnd (== '/') iss <> "/"

-- End - Configuration loading system

-- Begin - Functions to handle a Config
---------------------------------------

-- | Simply returns the config workspaces
getWorkspaces :: Config -> [Index]
getWorkspaces Config {..} = workspaces

-- End - Functions to handle a Config

-- Begin - Functions to handle an Index
---------------------------------------

-- | Get the 'Index' name
getWorkspaceName :: Index -> Text
getWorkspaceName Index {..} = name

-- | Find an 'Index' by name
lookupTenant :: [Index] -> Text -> Maybe Index
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
lookupCrawler index crawlerName = find isProject (crawlers index)
  where
    isProject Crawler {..} = name == crawlerName

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

-- End - Functions to handle a Crawler

-- Begin - Some utility functions
---------------------------------

-- | Create an empty 'Index'
mkTenant :: Text -> Index
mkTenant name =
  Index
    { name,
      crawlers = [],
      crawlers_api_key = Nothing,
      projects = Nothing,
      idents = Nothing,
      search_aliases = Nothing
    }

-- | Get 'Ident' ident from a list of 'Ident'
getIdentByAliasFromIdents :: Text -> [Ident] -> Maybe Text
getIdentByAliasFromIdents alias idents' = case find isMatched idents' of
  Nothing -> Nothing
  Just Ident {..} -> Just ident
  where
    isMatched :: Ident -> Bool
    isMatched Ident {..} = alias `elem` aliases

-- End - Some utility functions
