-- | The library environment and logging functions
module Monocle.Env where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.Bloodhound as BH
import qualified Database.Bloodhound.Raw as BHR
import GHC.Stack (srcLocFile, srcLocStartLine)
import qualified Json.Extras as Json
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (QueryRequest_QueryType (..))
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (Expr)
import qualified Network.HTTP.Client as HTTP
import Say
import qualified Servant (Handler)
import qualified System.Log.FastLogger as FastLogger

-------------------------------------------------------------------------------
-- The main AppM context, embeded in the Servant handler
-------------------------------------------------------------------------------
data Env = Env
  { bhEnv :: BH.BHEnv,
    glLogger :: Logger
  }

-- | 'Env' is the global environment
data AppEnv = AppEnv
  { config :: IO [Config.Index],
    aEnv :: Env
  }

-- | 'AppM' is the main context, it just adds Env to the servant Handler using Reader
newtype AppM a = AppM {unApp :: ReaderT AppEnv Servant.Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadReader AppEnv)

-- | We can derive a MonadBH from AppM, we just needs to tell 'getBHEnv' where is BHEnv
instance BH.MonadBH AppM where
  getBHEnv = asks (bhEnv . aEnv)

instance MonadFail AppM where
  fail = error . toText

-- | 'getConfig' reload the config automatically from the env
getConfig :: AppM [Config.Index]
getConfig = do
  loadConfig <- asks config
  --  logger <- asks (glLogger . aEnv)
  --  let logReload = (logEvent logger . ReloadConfig)
  liftIO loadConfig

-------------------------------------------------------------------------------
-- The query context, associated to each individual http request
-------------------------------------------------------------------------------

-- | 'QueryEnv' is the request environment, after validation
data QueryEnv = QueryEnv
  { -- | The current workspace configuration (todo: rename tenant and Index into Workspace)
    tenant :: Config.Index,
    -- | The application env, for logging and accessing bloodhound
    tEnv :: Env,
    -- | The query language expression, used by the `withXXX` combinator below
    tQuery :: Q.Query,
    -- | An optional context label, used by the Query.measure profiler
    tContext :: Maybe Text
  }

-- | 'QueryM' is a concret newtype so that we get better error message,
--   but it is really a simpler Reader over IO
newtype QueryM a = QueryM {unTenant :: ReaderT QueryEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)
  deriving newtype (MonadReader QueryEnv)
  deriving newtype (MonadUnliftIO)

-- | We can derive a MonadBH from QueryM, we just needs to read the BHEnv from the tEnv
instance BH.MonadBH QueryM where
  getBHEnv = QueryM (asks $ bhEnv . tEnv)

-- | The QueryMonad is the main constraint used by the Queries module. Note that it implies ElasticMonad.
class (Functor m, Applicative m, Monad m, MonadReader QueryEnv m, ElasticMonad m) => QueryMonad m where
  -- | Get the current (UTC) time.
  getCurrentTime' :: m UTCTime

  -- | Log a debug
  trace' :: Text -> m ()

instance QueryMonad QueryM where
  getCurrentTime' = liftIO getCurrentTime
  trace' msg = liftIO $ say msg

-- | A type class that defines the method available to query elastic
class ElasticMonad m where
  elasticSearch :: (ToJSON body, FromJSON resp) => BH.IndexName -> body -> BHR.ScrollRequest -> m (BH.SearchResult resp)
  elasticCountByIndex :: BH.IndexName -> BH.CountQuery -> m (Either BH.EsError BH.CountResponse)
  elasticSearchHit :: ToJSON body => BH.IndexName -> body -> m [Json.Value]
  elasticAdvance :: FromJSON resp => BH.ScrollId -> m (BH.SearchResult resp)

instance ElasticMonad QueryM where
  elasticSearch = BHR.search
  elasticSearchHit = BHR.searchHit
  elasticAdvance = BHR.advance
  elasticCountByIndex = BH.countByIndex

-- | Run a QueryM computation without a Query, e.g. when adding task data.
runEmptyQueryM :: Config.Index -> QueryM a -> AppM a
runEmptyQueryM = flip runQueryM (mkQuery [])

-- | Run a QueryM in the AppM
runQueryM :: Config.Index -> Q.Query -> QueryM a -> AppM a
runQueryM tenant tQuery (QueryM im) = do
  tEnv <- asks aEnv
  liftIO $ runReaderT im (QueryEnv {..})
  where
    tContext = Nothing

-- | Run a 'QueryM' with an existing BHEnv
runQueryM' :: forall a. BH.BHEnv -> Config.Index -> QueryM a -> IO a
runQueryM' bhEnv tenant tenantM = withLogger $ \glLogger ->
  let tEnv = Env {..}
      tQuery = mkQuery []
      tContext = Nothing
   in runReaderT (unTenant tenantM) (QueryEnv {..})

-- | Create the bloodhound environment
mkEnv :: MonadIO m => Text -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

mkEnv' :: MonadIO m => m BH.BHEnv
mkEnv' = do
  url <- fromMaybe "http://localhost:9200" <$> lookupEnv "ELASTIC_URL"
  mkEnv (toText url)

-- | Run a QueryM without sharing a BHEnv, this is useful for one-off test
testQueryM :: Config.Index -> QueryM a -> IO a
testQueryM config tenantM = do
  url <- fromMaybe "http://localhost:9200" <$> lookupEnv "ELASTIC_URL"
  bhEnv <- mkEnv (toText url)
  runQueryM' bhEnv config tenantM

-- | Re-export utility function to create a config for testQueryM
mkConfig :: Text -> Config.Index
mkConfig name = Config.defaultTenant name

-- | Utility function to hide the ReaderT layer
getIndexName :: QueryMonad m => m BH.IndexName
getIndexName = tenantIndexName <$> asks tenant
  where
    tenantIndexName :: Config.Index -> BH.IndexName
    tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> name

-- | Utility function to hide the ReaderT layer
getIndexConfig :: QueryMonad m => m Config.Index
getIndexConfig = asks tenant

-- | Utility function to hide the ReaderT layer
getQuery :: QueryMonad m => m Q.Query
getQuery = asks tQuery

getContext :: QueryMonad m => m (Maybe Text)
getContext = asks tContext

getQueryBH :: QueryMonad m => m (Maybe BH.Query)
getQueryBH = mkFinalQuery Nothing

-- | 'mkQuery' creates a Q.Query from a BH.Query
mkQuery :: [BH.Query] -> Q.Query
mkQuery bhq =
  let queryGet _mod _flavor = bhq
      queryBounds = (error "no bound", error "no bound")
      queryMinBoundsSet = False
   in Q.Query {..}

mkFinalQuery :: QueryMonad m => Maybe Q.QueryFlavor -> m (Maybe BH.Query)
mkFinalQuery flavorM = do
  query <- getQuery
  pure $ toBoolQuery $ Q.queryGet query id flavorM
  where
    toBoolQuery = \case
      [] -> Nothing
      [x] -> Just x
      xs -> Just $ BH.QueryBoolQuery $ BH.mkBoolQuery [] (BH.Filter <$> xs) [] []

withQuery :: QueryMonad m => Q.Query -> m a -> m a
withQuery query = local addQuery
  where
    addQuery e = e {tQuery = query}

withContext :: QueryMonad m => HasCallStack => Text -> m a -> m a
withContext context = local setContext
  where
    setContext (QueryEnv tenant tEnv query _) = (QueryEnv tenant tEnv query (Just contextName))
    contextName = maybe context getLoc $ headMaybe (getCallStack callStack)
    getLoc (_, loc) = "[" <> context <> " " <> toText (srcLocFile loc) <> ":" <> show (srcLocStartLine loc) <> "]"

-- | 'dropQuery' remove the query from the context
dropQuery :: QueryMonad m => m a -> m a
dropQuery = local dropQuery'
  where
    -- we still want to call the provided modifier, so
    -- the expr is removed by discarding the modifier parameter
    dropQuery' (QueryEnv tenant tEnv query context) =
      let newQueryGet modifier = Q.queryGet query (const $ modifier Nothing)
       in QueryEnv tenant tEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withFlavor' change the query flavor
withFlavor :: QueryMonad m => Q.QueryFlavor -> m a -> m a
withFlavor flavor = local setFlavor
  where
    -- the new flavor replaces the oldFlavor
    setFlavor (QueryEnv tenant tEnv query context) =
      let newQueryGet modifier oldFlavor = Q.queryGet query modifier (Just $ fromMaybe flavor oldFlavor)
       in QueryEnv tenant tEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withModified' run a queryM with a modified query
-- Use it to remove or change field from the initial expr, for example to drop dates.
withModified :: QueryMonad m => (Maybe Expr -> Maybe Expr) -> m a -> m a
withModified modifier = local addModifier
  where
    -- The new modifier is composed with the previous one
    addModifier (QueryEnv tenant tEnv query context) =
      let newQueryGet oldModifier qf = Q.queryGet query (modifier . oldModifier) qf
       in QueryEnv tenant tEnv (query {Q.queryGet = newQueryGet}) context

-- | 'withFilter' run a queryM with extra queries.
-- Use it to mappend bloodhound expression to the final result
withFilter :: QueryMonad m => [BH.Query] -> m a -> m a
withFilter extraQueries = local addFilter
  where
    -- The extra query is added to the resulting [BH.Query]
    addFilter (QueryEnv tenant tEnv query context) =
      let newQueryGet modifier qf = extraQueries <> Q.queryGet query modifier qf
       in QueryEnv tenant tEnv (query {Q.queryGet = newQueryGet}) context

data Entity = Project Text | Organization Text | TaskDataEntity
  deriving (Eq, Show)

instance From Entity Text where
  from = \case
    Project _ -> "project"
    Organization _ -> "organization"
    TaskDataEntity -> "task_datas"

instance From Entity LText where
  from = via @Text

getEntityName :: Entity -> Text
getEntityName = \case
  Project n -> n
  Organization n -> n
  TaskDataEntity -> ""

-------------------------------------------------------------------------------
-- logging function
-------------------------------------------------------------------------------
type Logger = FastLogger.TimedFastLogger

-- | withLogger create the logger
--
-- try with repl:
-- λ> runQueryM' Prelude.undefined (Config.defaultTenant "tenant") $ monocleLogEvent (AddingChange "test" 42 42)
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

monocleLogEvent :: MonocleEvent -> QueryM ()
monocleLogEvent ev = do
  Config.Index {..} <- getIndexConfig
  logger <- asks (glLogger . tEnv)
  liftIO $ doLog logger (encodeUtf8 $ name <> ": " <> eventToText ev)
