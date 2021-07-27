-- | The library environment and logging functions
module Monocle.Env where

import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (QueryRequest_QueryType (..))
import qualified Monocle.Search.Query as Q
import Servant (Handler)

-------------------------------------------------------------------------------
-- context monads and utility functions

-- | 'Env' is the global environment
data Env = Env
  { config :: IORef Config.ReloadableConfig,
    bhEnv :: BH.BHEnv
  }

-- | 'AppM' is the main context, it just adds Env to the servant Handler using Reader
newtype AppM a = AppM {unApp :: ReaderT Env Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadReader Env)

-- | 'getConfig' reload the config automatically from the env
getConfig :: AppM [Config.Index]
getConfig = Config.reloadConfig =<< asks config

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

data Entity = Project {getName :: Text} | Organization {getName :: Text}
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- logging function
data MonocleEvent
  = AddingChange LText Int Int
  | AddingProject Text Text Int
  | UpdatingEntity LText Entity UTCTime
  | Searching QueryRequest_QueryType LText Q.Query

eventToText :: MonocleEvent -> Text
eventToText ev = case ev of
  AddingChange crawler changes events ->
    toStrict crawler <> " adding " <> show changes <> " changes with " <> show events <> " events"
  AddingProject crawler organizationName projects ->
    crawler <> " adding " <> show projects <> " changes for organization" <> organizationName
  UpdatingEntity crawler entity ts ->
    toStrict crawler <> " updating " <> show entity <> " to " <> show ts
  Searching queryType queryText query ->
    let jsonQuery = decodeUtf8 . encode $ Q.queryBH query Q.defaultQueryFlavor
     in "searching " <> show queryType <> " with `" <> toStrict queryText <> "`: " <> jsonQuery

monocleLogEvent :: MonocleEvent -> TenantM ()
monocleLogEvent ev = do
  Config.Index {..} <- getIndexConfig
  sayErr $ name <> ": " <> eventToText ev
