-- | The library environment and logging functions
module Monocle.Env where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.Bloodhound as BH
import GHC.Stack (srcLocFile, srcLocStartLine)
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (QueryRequest_QueryType (..))
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (Expr)
import qualified Network.HTTP.Client as HTTP
import Servant (Handler)
import qualified System.Log.FastLogger as FastLogger

-------------------------------------------------------------------------------
-- context monads and utility functions

data Env = Env
  { bhEnv :: BH.BHEnv,
    glLogger :: Logger
  }

-- | 'Env' is the global environment
data AppEnv = AppEnv
  { config :: IORef Config.ReloadableConfig,
    aEnv :: Env
  }

-- | 'AppM' is the main context, it just adds Env to the servant Handler using Reader
newtype AppM a = AppM {unApp :: ReaderT AppEnv Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadReader AppEnv)

-- | 'getConfig' reload the config automatically from the env
getConfig :: AppM [Config.Index]
getConfig = do
  logger <- asks (glLogger . aEnv)
  Config.reloadConfig (logEvent logger . ReloadConfig) =<< asks config

-- | 'TenantEnv' is the request environment, after validation
data TenantEnv = TenantEnv
  { tenant :: Config.Index,
    tEnv :: Env
  }

-- | 'TenantM' is the request context, it contains the TenantEnv
newtype TenantM a = TenantM {unTenant :: ReaderT TenantEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)
  deriving newtype (MonadReader TenantEnv)
  deriving newtype (MonadUnliftIO)

tenantIndexName :: Config.Index -> BH.IndexName
tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> name

-- | 'runTenantM' is the main way to run a 'TenantM' computation
runTenantM :: Config.Index -> TenantM a -> AppM a
runTenantM tenant (TenantM im) = do
  tEnv <- asks aEnv
  liftIO $ runReaderT im (TenantEnv {..})

-- | 'runTenantM'' run a 'TenantM' with an existing BHEnv
runTenantM' :: forall a. BH.BHEnv -> Config.Index -> TenantM a -> IO a
runTenantM' bhEnv config tenantM = withLogger $ \glLogger ->
  runReaderT (unTenant tenantM) (TenantEnv config Env {..})

mkEnv :: MonadIO m => Text -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

-- | 'testTenantM' run a TenantM using the ELASTIC_URL environment variable
-- Use this to test a tenant action from the repl.
testTenantM :: Config.Index -> TenantM a -> IO a
testTenantM config tenantM = do
  url <- fromMaybe "http://localhost:9200" <$> lookupEnv "ELASTIC_URL"
  bhEnv <- mkEnv (toText url)
  withLogger $ \glLogger ->
    runReaderT (unTenant tenantM) (TenantEnv config Env {..})

-- | Re-export utility function to create a config for testTenantM
mkConfig :: Text -> Config.Index
mkConfig name = Config.defaultTenant name

-- | We can derive a MonadBH from AppM, we just needs to tell 'getBHEnv' where is BHEnv
instance BH.MonadBH AppM where
  getBHEnv = asks (bhEnv . aEnv)

-- | We can also derive a MonadBH from TenantM, we just needs to lift to the parent Reader
instance BH.MonadBH TenantM where
  getBHEnv = TenantM (asks $ bhEnv . tEnv)

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

-- | 'mkQuery' creates a Q.Query from a BH.Query
mkQuery :: Maybe BH.Query -> Q.Query
mkQuery bhq =
  let queryGet _mod _flavor = maybeToList bhq
      queryBounds = (error "no bound", error "no bound")
      queryMinBoundsSet = False
   in Q.Query {..}

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
type Logger = FastLogger.TimedFastLogger

-- | withLogger create the logger
--
-- try with repl:
-- λ> runTenantM' Prelude.undefined (Config.defaultTenant "tenant") $ monocleLogEvent (AddingChange "test" 42 42)
withLogger :: (Logger -> IO a) -> IO a
withLogger cb = do
  tc <- liftIO $ FastLogger.newTimeCache "%F %T "
  FastLogger.withTimedFastLogger tc logger cb
  where
    logger = FastLogger.LogStderr 1024

doLog :: Logger -> ByteString -> IO ()
doLog logger message = logger (\time -> FastLogger.toLogStr $ time <> message <> "\n")

data SystemEvent
  = Ready Int Int Text
  | ReloadConfig FilePath

sysEventToText :: SystemEvent -> ByteString
sysEventToText = \case
  Ready tenantCount port url ->
    "Serving " <> show tenantCount <> " tenant(s) on 0.0.0.0:" <> show port <> " with elastic: " <> encodeUtf8 url
  ReloadConfig fp ->
    "Reloading " <> encodeUtf8 fp

logEvent :: Logger -> SystemEvent -> IO ()
logEvent logger ev = doLog logger (sysEventToText ev)

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
  logger <- asks (glLogger . tEnv)
  liftIO $ doLog logger (encodeUtf8 $ name <> ": " <> eventToText ev)
