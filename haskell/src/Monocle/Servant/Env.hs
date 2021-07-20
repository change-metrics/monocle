-- | The api configuration environment
module Monocle.Servant.Env
  ( -- * AppM : the global environment
    Env (..),
    AppM (..),

    -- * TenantM : the request environment
    TenantM,
    runTenantM,
    runTenantM',
    tenantIndexName,
    getIndexName,
    getIndexConfig,

    -- * QueryM : the query environment
    QueryM,
    runQueryM,
    getQuery,
    getQueryBH,
    getQueryBHWithFlavor,
    mkFinalQuery,
    liftTenantM,
    withFilter,
    runTenantQueryM,
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import qualified Monocle.Search.Syntax as Q
import Relude
import Servant (Handler)

-- | 'Env' is the global environment
data Env = Env
  { tenants :: [Config.Index],
    bhEnv :: BH.BHEnv
  }

-- | 'AppM' is the main context, it just adds Env to the servant Handler using Reader
newtype AppM a = AppM {unApp :: ReaderT Env Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadReader Env)

-- | 'TenantEnv' is the request environment, after validation
data TenantEnv = TenantEnv
  { tenant :: Config.Index,
    bhEnv' :: BH.BHEnv
  }

-- | 'TenantM' is the request context, it contains the TenantEnv
newtype TenantM a = TenantM {unTenant :: ReaderT TenantEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)
  deriving newtype (MonadReader TenantEnv)

tenantIndexName :: Config.Index -> BH.IndexName
tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> name

-- | 'runTenantM' is the only way to run an 'TenantM' computation
runTenantM :: Config.Index -> TenantM a -> AppM a
runTenantM config (TenantM im) = do
  bhEnv <- BH.getBHEnv
  liftIO $ runReaderT im (TenantEnv config bhEnv)

-- | 'runTenantM'' is used in test, without the servant Handler
runTenantM' :: forall a. BH.BHEnv -> Config.Index -> TenantM a -> IO a
runTenantM' bhEnv config tenantM =
  runReaderT (unTenant tenantM) (TenantEnv config bhEnv)

-- | We can derive a MonadBH from AppM, we just needs to tell 'getBHEnv' where is BHEnv
instance BH.MonadBH AppM where
  getBHEnv = asks bhEnv

-- | We can also derive a MonadBH from TenantM, we just needs to lift to the parent Reader
instance BH.MonadBH TenantM where
  getBHEnv = TenantM (asks bhEnv')

getIndexName :: TenantM BH.IndexName
getIndexName = tenantIndexName <$> asks tenant

getIndexConfig :: TenantM Config.Index
getIndexConfig = asks tenant

instance MonadFail AppM where
  fail = error . toText

-- | 'QueryM' is the query context
type QueryM = ReaderT Q.Query TenantM

-- | 'runQueryM' run the query context
runQueryM :: Q.Query -> QueryM a -> TenantM a
runQueryM query qm = runReaderT qm query

-- | 'runTenantQueryM' combine runTenantM and runQueryM
runTenantQueryM :: Config.Index -> Q.Query -> QueryM a -> AppM a
runTenantQueryM config query qm = runTenantM config (runQueryM query qm)

-- | 'getQuery' provides the query from the context
getQuery :: QueryM Q.Query
getQuery = ask

mkFinalQuery :: [BH.Query] -> Maybe BH.Query
mkFinalQuery = \case
  [] -> Nothing
  [x] -> Just x
  xs -> Just $ BH.QueryBoolQuery $ BH.mkBoolQuery xs [] [] []

getQueryBHWithFlavor :: Q.QueryFlavor -> QueryM (Maybe BH.Query)
getQueryBHWithFlavor flavor = do
  query <- getQuery
  pure $ mkFinalQuery $ Q.queryBH query flavor

getQueryBH :: QueryM (Maybe BH.Query)
getQueryBH = getQueryBHWithFlavor Q.defaultQueryFlavor

-- | 'liftTenantM' run a TenantM in the QueryM
liftTenantM :: TenantM a -> QueryM a
liftTenantM = lift

-- | 'withFilter' run a queryM with a modified filter query
withFilter :: [BH.Query] -> QueryM a -> QueryM a
withFilter extraQueries = local addFilter
  where
    addFilter query =
      -- create a new queryBH
      let newQueryBH qf = extraQueries <> Q.queryBH query qf
       in query {Q.queryBH = newQueryBH}
