-- | The library environment and logging functions
module Monocle.Env where

import Database.Bloodhound qualified as BH
import Monocle.Api.Jwt (OIDCEnv)
import Monocle.Config qualified as Config
import Monocle.Prelude
import Monocle.Search.Query qualified as Q
import Network.HTTP.Client qualified as HTTP
import Servant.Auth.Server (JWTSettings)

-------------------------------------------------------------------------------
-- The main AppEnv context, embeded in the Servant handler
-------------------------------------------------------------------------------
data OIDC = OIDC
  { oidcEnv :: Maybe OIDCEnv
  , localJWTSettings :: JWTSettings
  }

-- | 'Env' is the global environment
data AppEnv = AppEnv
  { config :: IO Config.ConfigStatus
  , bhEnv :: BH.BHEnv
  , aOIDC :: OIDC
  }

-------------------------------------------------------------------------------
-- The query context, associated to each individual http request
-------------------------------------------------------------------------------

-- | 'QueryTarget' is the target of the query.
data QueryTarget
  = -- | It's either a single workspace
    QueryWorkspace Config.Index
  | -- | Or the whole config (e.g. for maintainance operation)
    QueryConfig Config.Config

-- | Create the bloodhound environment
mkEnv :: MonadIO m => Text -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

mkEnv' :: MonadIO m => m BH.BHEnv
mkEnv' = do
  url <- fromMaybe "http://localhost:9200" <$> lookupEnv "MONOCLE_ELASTIC_URL"
  mkEnv (from url)

-- | Re-export utility function to create a config for testQueryM
mkConfig :: Text -> Config.Index
mkConfig = Config.mkTenant

envToIndexName :: QueryTarget -> BH.IndexName
envToIndexName target = do
  case target of
    QueryWorkspace ws -> tenantIndexName ws
    QueryConfig _ -> BH.IndexName "monocle.config"
 where
  tenantIndexName :: Config.Index -> BH.IndexName
  tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> name

-- | 'mkQuery' creates a Q.Query from a BH.Query
mkQuery :: [BH.Query] -> Q.Query
mkQuery bhq =
  let queryGet _mod _flavor = bhq
      queryBounds = (error "no bound", error "no bound")
      queryMinBoundsSet = False
   in Q.Query {..}

mkFinalQuery :: Maybe Q.QueryFlavor -> Q.Query -> Maybe BH.Query
mkFinalQuery flavorM query = toBoolQuery $ Q.queryGet query id flavorM
 where
  toBoolQuery = \case
    [] -> Nothing
    [x] -> Just x
    xs -> Just $ BH.QueryBoolQuery $ BH.mkBoolQuery [] (BH.Filter <$> xs) [] []
