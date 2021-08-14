-- | The library environment and logging functions
module Monocle.Env where

import qualified Database.Bloodhound as BH
import GHC.Stack (srcLocFile, srcLocStartLine)
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (QueryRequest_QueryType (..))
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (Expr)
import qualified Network.HTTP.Client as HTTP
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
-- TODO: replace usage with testTenantM
runTenantM' :: forall a. BH.BHEnv -> Config.Index -> TenantM a -> IO a
runTenantM' bhEnv config tenantM =
  runReaderT (unTenant tenantM) (TenantEnv config bhEnv)

mkEnv :: MonadIO m => Text -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

-- | 'testTenantM' run a TenantM using the ELASTIC_URL environment variable
testTenantM :: Config.Index -> TenantM a -> IO a
testTenantM config tenantM = do
  url <- getEnv' "ELASTIC_URL"
  bhEnv <- mkEnv url
  runTenantM' bhEnv config tenantM

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
data QueryEnv = QueryEnv
  { qeQuery :: Q.Query,
    qeContext :: Maybe Text
  }

type QueryM = ReaderT QueryEnv TenantM

-- | 'runQueryM' run the query context
runQueryM :: Q.Query -> QueryM a -> TenantM a
runQueryM query qm = runReaderT qm (QueryEnv query Nothing)

-- | 'runTenantQueryM' combine runTenantM and runQueryM
runTenantQueryM :: Config.Index -> Q.Query -> QueryM a -> AppM a
runTenantQueryM config query qm = runTenantM config (runQueryM query qm)

-- | 'getQuery' provides the query from the context
getQuery :: QueryM Q.Query
getQuery = asks qeQuery

getContext :: QueryM (Maybe Text)
getContext = asks qeContext

mkFinalQuery :: Maybe Q.QueryFlavor -> QueryM (Maybe BH.Query)
mkFinalQuery flavorM = do
  query <- getQuery
  pure $ toBoolQuery $ Q.queryGet query id flavorM
  where
    toBoolQuery = \case
      [] -> Nothing
      [x] -> Just x
      xs -> Just $ BH.QueryBoolQuery $ BH.mkBoolQuery [] (BH.Filter <$> xs) [] []

getQueryBH :: QueryM (Maybe BH.Query)
getQueryBH = mkFinalQuery Nothing

-- | 'liftTenantM' run a TenantM in the QueryM
liftTenantM :: TenantM a -> QueryM a
liftTenantM = lift

withContext :: HasCallStack => Text -> QueryM a -> QueryM a
withContext context = local setContext
  where
    setContext (QueryEnv query _) = (QueryEnv query (Just contextName))
    contextName = maybe context getLoc $ headMaybe (getCallStack callStack)
    getLoc (_, loc) = "[" <> context <> " " <> toText (srcLocFile loc) <> ":" <> show (srcLocStartLine loc) <> "]"

-- | 'dropQuery' remove the query from the context
dropQuery :: QueryM a -> QueryM a
dropQuery = local dropQuery'
  where
    -- we still want to call the provided modifier, so
    -- the expr is removed by discarding the modifier parameter
    dropQuery' (QueryEnv query context) =
      let newQueryGet modifier = Q.queryGet query (const $ modifier Nothing)
       in QueryEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withFlavor' change the query flavor
withFlavor :: Q.QueryFlavor -> QueryM a -> QueryM a
withFlavor flavor = local setFlavor
  where
    -- the new flavor replaces the oldFlavor
    setFlavor (QueryEnv query context) =
      let newQueryGet modifier oldFlavor = Q.queryGet query modifier (Just $ fromMaybe flavor oldFlavor)
       in QueryEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withModified' run a queryM with a modified query
-- Use it to remove or change field from the initial expr, for example to drop dates.
withModified :: (Maybe Expr -> Maybe Expr) -> QueryM a -> QueryM a
withModified modifier = local addModifier
  where
    -- The new modifier is composed with the previous one
    addModifier (QueryEnv query context) =
      let newQueryGet oldModifier qf = Q.queryGet query (modifier . oldModifier) qf
       in QueryEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withFilter' run a queryM with extra queries.
-- Use it to mappend bloodhound expression to the final result
withFilter :: [BH.Query] -> QueryM a -> QueryM a
withFilter extraQueries = local addFilter
  where
    -- The extra query is added to the resulting [BH.Query]
    addFilter (QueryEnv query context) =
      let newQueryGet modifier qf = extraQueries <> Q.queryGet query modifier qf
       in QueryEnv (query {Q.queryGet = newQueryGet}) context

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
    let jsonQuery = decodeUtf8 . encode $ Q.queryGet query id Nothing
     in "searching " <> show queryType <> " with `" <> toStrict queryText <> "`: " <> jsonQuery

monocleLogEvent :: MonocleEvent -> TenantM ()
monocleLogEvent ev = do
  Config.Index {..} <- getIndexConfig
  sayErr $ name <> ": " <> eventToText ev
