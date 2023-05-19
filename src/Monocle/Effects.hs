{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

-- for MTL Compat

-- | This module demonstrates how the static reader based effect
-- provided by the effectful library can be used to replace the
-- current mtl style solution.
--
-- Note: [Monocle Effects]
--
-- Monocle uses effectful to implement a simple effect system.
-- An effect is composed of an environment data type and associated
-- functions.
--
-- The goals are:
--
-- - Remove the `instance HasLogger Api` and `instance HasLogger Worker` boilerplate.
--   The effects should be usable and general purpose.
-- - Enable using multiple reader so that effect can easily have an environment.
--   This removes the needs for `AppEnv.aEnv.glLogger` and `QueryEnv.tEnv.glLogger`
-- - Keep IO out of the main module, the signature should indicate precisely what are
--   the necessary effects. E.g. crawler shall not be able to access elastic.
--
-- Design:
--
-- The effect are implemented using the familiar `ReaderT env IO` but using a StaticRep and 'unsafeEff' to liftIO.
-- The downside is that dynamic implementation, e.g. for testing, is presently not possible.
--
-- Effectful uses an extensible record indexed by types of kind Effect.
-- This list is represented as a type variable, usually named `es`
--
-- * Effect constraints
--
-- Effect can be added as a contraints using the `:>` operator:
--
--   `LoggerEffect :> es` meanst that the es list contains the Effect.
--
-- * Effect execution
--
-- Effect can be executed using a run* function to remove the effect from the list.
--
-- When executing the final effects, constraint can't be used,
-- the effects need to be concrete and in order:
--
-- runMyEffect :: Eff [LoggerEffect, Fail, IOE] a -> IO (Either String a)
-- runMyEffect = runEff . runFail . runLoggerE
--
-- This does not work: `[LoggerEffect, Fail, IOE] :>> es => Eff es a -> IO a`
--
-- As a rule of thumb, when IO is on the right hand side of the arrow, then the
-- Effect needs to be defined as concrete Eff type.
--
-- Moreover, when peeling just one effect, then the peeled effect needs to be concret too:
--
-- runFail :: Eff (Fail : es) a => Eff es (Either String a)
module Monocle.Effects where

import Monocle.Prelude hiding (Reader, ask, local)

import Control.Exception (finally)
import Monocle.Client qualified
import Monocle.Config qualified
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Client qualified as HTTP

import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, localStaticRep)
import Effectful.Dispatch.Static.Primitive qualified as EffStatic
import Monocle.Effects.Compat ()

import GHC.IO.Handle (hClose)
import Monocle.Config (ConfigStatus)
import System.Directory
import System.Posix.Temp (mkstemp)
import Test.Tasty
import Test.Tasty.HUnit

import Network.Wai.Handler.Warp qualified as Warp
import Servant (Get, (:<|>))
import Servant qualified

import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Database.Bloodhound.Raw qualified as BHR
import Json.Extras qualified as Json

-- for MonoQuery

import Monocle.Env (AppEnv)
import Monocle.Env qualified
import Monocle.Search.Query qualified as SearchQuery
import Monocle.Search.Syntax (Expr)

import Effectful.Error.Static qualified as E
import Effectful.Fail qualified as E
import Effectful.Prometheus
import Effectful.Reader.Static qualified as E
import Effectful.Retry as Retry
import Monocle.Client (MonocleClient)
import Monocle.Client.Api (crawlerAddDoc, crawlerCommit, crawlerCommitInfo)

import Monocle.Protob.Crawler qualified as CrawlerPB

-- the servant api, previously known as AppM
type ApiEffects es =
  ( IOE :> es
  , E.Reader AppEnv :> es
  , E.Error Servant.ServerError :> es
  , MonoConfigEffect :> es
  , LoggerEffect :> es
  , ElasticEffect :> es
  , E.Fail :> es
  )

-- the effect necessary to run elastic request
type IndexEffects es = (ElasticEffect :> es, LoggerEffect :> es)

-- the query handler :> es, previously known as QueryM
type QEffects es = (ElasticEffect :> es, LoggerEffect :> es, MonoQuery :> es)

-- the macro handler :> es, previously known as LentilleM
type CrawlerEffects es = (LoggerEffect :> es, MonoClientEffect :> es)

type TestEffects es = (E.Fail :> es, IOE :> es, QEffects es)

testTree :: TestTree
testTree =
  testGroup
    "Monocle.Effects"
    [ testCase "LoggerEffect" do
        runEff $ runLoggerEffect do
          logInfo "logInfo prints!" []
    , testCase "MonoConfig" do
        (path, fd) <- mkstemp "/tmp/monoconfig-test"
        hClose fd
        writeFile path "workspaces: []"
        setEnv "CRAWLERS_API_KEY" "42"
        runEff (runMonoConfig path $ testMonoConfig path) `finally` removeFile path
    ]
 where
  testEff a b = liftIO (a @?= b)
  testMonoConfig :: (MonoConfigEffect :> es, IOE :> es) => FilePath -> Eff es ()
  testMonoConfig fp = do
    -- Setup the test config
    let getNames c = Monocle.Config.getWorkspaceName <$> Monocle.Config.getWorkspaces (Monocle.Config.csConfig c)

    -- initial load
    do
      config <- getReloadConfig
      Monocle.Config.csReloaded config `testEff` False
      getNames config `testEff` []

    -- test reload works
    do
      liftIO do writeFile fp "workspaces:\n- name: test\n  crawlers: []"
      config <- getReloadConfig
      Monocle.Config.csReloaded config `testEff` True
      getNames config `testEff` ["test"]

    -- make sure reload is avoided when the file doesn't change
    do
      config <- getReloadConfig
      Monocle.Config.csReloaded config `testEff` False

------------------------------------------------------------------
--

-- | Config effect to load and reload the local config

------------------------------------------------------------------

-- | The effect environment
type MonoConfigEnv = IO ConfigStatus

-- | The effect definition using static rep.
data MonoConfigEffect :: Effect

type instance DispatchOf MonoConfigEffect = 'Static 'WithSideEffects
newtype instance StaticRep MonoConfigEffect = MonoConfigEffect MonoConfigEnv

-- | Run the effect (e.g. removes it from the list)
runMonoConfig :: IOE :> es => FilePath -> Eff (MonoConfigEffect : es) a -> Eff es a
runMonoConfig fp action = do
  (mkReload :: IO ConfigStatus) <- unsafeEff_ (Monocle.Config.reloadConfig fp)
  evalStaticRep (MonoConfigEffect mkReload) action

runMonoConfigFromEnv :: IOE :> es => IO ConfigStatus -> Eff (MonoConfigEffect : es) a -> Eff es a
runMonoConfigFromEnv reload = evalStaticRep (MonoConfigEffect reload)

-- | The lifted version of Monocle.Config.reloadConfig
getReloadConfig :: MonoConfigEffect :> es => Eff es ConfigStatus
getReloadConfig = do
  MonoConfigEffect reload <- getStaticRep
  unsafeEff_ reload

------------------------------------------------------------------
--

-- | Monocle API Client

------------------------------------------------------------------
type MonoClientEnv = MonocleClient

data MonoClientEffect :: Effect
type instance DispatchOf MonoClientEffect = 'Static 'WithSideEffects
newtype instance StaticRep MonoClientEffect = MonoClientEffect MonoClientEnv

runMonoClient :: IOE :> es => MonocleClient -> Eff (MonoClientEffect : es) a -> Eff es a
runMonoClient client = evalStaticRep (MonoClientEffect client)

mCrawlerCommitInfo :: MonoClientEffect :> es => CrawlerPB.CommitInfoRequest -> Eff es CrawlerPB.CommitInfoResponse
mCrawlerCommitInfo req = do
  MonoClientEffect env <- getStaticRep
  unsafeEff_ $ crawlerCommitInfo env req

mCrawlerCommit :: MonoClientEffect :> es => CrawlerPB.CommitRequest -> Eff es CrawlerPB.CommitResponse
mCrawlerCommit req = do
  MonoClientEffect env <- getStaticRep
  unsafeEff_ $ crawlerCommit env req

mCrawlerAddDoc :: MonoClientEffect :> es => CrawlerPB.AddDocRequest -> Eff es CrawlerPB.AddDocResponse
mCrawlerAddDoc req = do
  MonoClientEffect env <- getStaticRep
  unsafeEff_ $ crawlerAddDoc env req

------------------------------------------------------------------
--

-- | Query effects

------------------------------------------------------------------
data MonoQueryEnv = MonoQueryEnv
  { queryTarget :: Monocle.Env.QueryTarget
  , searchQuery :: SearchQuery.Query
  }

data MonoQuery :: Effect
type instance DispatchOf MonoQuery = 'Static 'NoSideEffects
newtype instance StaticRep MonoQuery = MonoQuery MonoQueryEnv

runMonoQuery :: MonoQueryEnv -> Eff (MonoQuery : es) a -> Eff es a
runMonoQuery env = evalStaticRep (MonoQuery env)

runMonoQueryConfig :: SearchQuery.Query -> Eff (MonoQuery : es) a -> Eff es a
runMonoQueryConfig q = evalStaticRep (MonoQuery $ MonoQueryEnv (Monocle.Env.QueryWorkspace $ Monocle.Config.mkTenant "test-tenant") q)

runQueryM :: Monocle.Config.Index -> SearchQuery.Query -> Eff (MonoQuery : es) a -> Eff es a
runQueryM ws query = evalStaticRep (MonoQuery $ MonoQueryEnv target query)
 where
  target = Monocle.Env.QueryWorkspace ws

runEmptyQueryM :: Monocle.Config.Index -> Eff (MonoQuery : es) a -> Eff es a
runEmptyQueryM ws = runQueryM ws query
 where
  query = Monocle.Env.mkQuery []

localSearchQuery :: MonoQuery :> es => (SearchQuery.Query -> SearchQuery.Query) -> Eff es a -> Eff es a
localSearchQuery changeQuery = localStaticRep updateRep
 where
  updateRep (MonoQuery env) = MonoQuery (env {searchQuery = changeQuery env.searchQuery})

localQueryTarget :: MonoQuery :> es => Monocle.Env.QueryTarget -> Eff es a -> Eff es a
localQueryTarget localTarget = localStaticRep updateRep
 where
  updateRep (MonoQuery env) = MonoQuery (env {queryTarget = localTarget})

withQuery :: MonoQuery :> es => SearchQuery.Query -> Eff es a -> Eff es a
withQuery query = localSearchQuery (const query)

-- | 'withFlavor' change the query flavor
withFlavor :: MonoQuery :> es => SearchQuery.QueryFlavor -> Eff es a -> Eff es a
withFlavor flavor = localSearchQuery setFlavor
 where
  -- the new flavor replaces the oldFlavor
  setFlavor query =
    let newQueryGet modifier oldFlavor = SearchQuery.queryGet query modifier (Just $ fromMaybe flavor oldFlavor)
     in query {SearchQuery.queryGet = newQueryGet}

-- | 'withFilter' run a queryM with extra queries.
-- Use it to mappend bloodhound expression to the final result
withFilter :: MonoQuery :> es => [BH.Query] -> Eff es a -> Eff es a
withFilter = localSearchQuery . addFilter

-- | 'withFilterFlavor' run a queryM with extra queries provided based on the current query flavor.
-- This is used in monoHisto where the extra bounds need to take into account the query flavor,
-- e.g. firstComment metrics uses
withFilterFlavor :: MonoQuery :> es => (Maybe SearchQuery.QueryFlavor -> [BH.Query]) -> Eff es a -> Eff es a
withFilterFlavor extraQueries = localSearchQuery addModifier
 where
  addModifier query =
    let newQueryGet modifier qf = extraQueries qf <> SearchQuery.queryGet query modifier qf
     in query {SearchQuery.queryGet = newQueryGet}

-- | 'withModified' run a queryM with a modified query
-- Use it to remove or change field from the initial expr, for example to drop dates.
withModified :: MonoQuery :> es => (Maybe Expr -> Maybe Expr) -> Eff es a -> Eff es a
withModified modifier = localSearchQuery addModifier
 where
  -- The new modifier is composed with the previous one
  addModifier query =
    let newQueryGet oldModifier = SearchQuery.queryGet query (modifier . oldModifier)
     in query {SearchQuery.queryGet = newQueryGet}

-- | Add extra queires to a QueryEnv
-- Extra queries are added to the resulting [BH.Query]
addFilter :: [BH.Query] -> SearchQuery.Query -> SearchQuery.Query
addFilter extraQueries query =
  let newQueryGet modifier qf = extraQueries <> SearchQuery.queryGet query modifier qf
   in query {SearchQuery.queryGet = newQueryGet}

-- | 'dropQuery' remove the query from the context
dropQuery :: MonoQuery :> es => Eff es a -> Eff es a
dropQuery = localSearchQuery dropQuery'
 where
  -- we still want to call the provided modifier, so
  -- the expr is removed by discarding the modifier parameter
  dropQuery' query =
    let newQueryGet modifier = SearchQuery.queryGet query (const $ modifier Nothing)
     in query {SearchQuery.queryGet = newQueryGet}

getQueryTarget :: MonoQuery :> es => Eff es Monocle.Env.QueryTarget
getQueryTarget = do
  MonoQuery env <- getStaticRep
  pure env.queryTarget

getIndexName :: MonoQuery :> es => Eff es BH.IndexName
getIndexName = do
  MonoQuery env <- getStaticRep
  pure $ Monocle.Env.envToIndexName (queryTarget env)

getIndexConfig :: MonoQuery :> es => Eff es Monocle.Config.Index
getIndexConfig = do
  MonoQuery env <- getStaticRep
  pure $ case queryTarget env of
    Monocle.Env.QueryWorkspace ws -> ws
    _ -> error "Config has no index config"

getQueryBH :: MonoQuery :> es => Eff es (Maybe BH.Query)
getQueryBH = do
  MonoQuery env <- getStaticRep
  pure $ Monocle.Env.mkFinalQuery Nothing env.searchQuery

getQueryBound :: MonoQuery :> es => Eff es (UTCTime, UTCTime)
getQueryBound = do
  MonoQuery env <- getStaticRep
  pure $ SearchQuery.queryBounds env.searchQuery

------------------------------------------------------------------
--

-- | Elastic Effect to access elastic backend.

------------------------------------------------------------------
type ElasticEnv = BH.BHEnv

data ElasticEffect :: Effect
type instance DispatchOf ElasticEffect = 'Static 'WithSideEffects
newtype instance StaticRep ElasticEffect = ElasticEffect ElasticEnv

runElasticEffect :: IOE :> es => BH.BHEnv -> Eff (ElasticEffect : es) a -> Eff es a
runElasticEffect bhEnv action = do
  -- bhEnv <- liftIO (BH.mkBHEnv <$> pure server <*> Monocle.Client.mkManager)
  evalStaticRep (ElasticEffect bhEnv) action

esSearch :: (ElasticEffect :> es, ToJSON body, FromJSONField resp) => BH.IndexName -> body -> BHR.ScrollRequest -> Eff es (BH.SearchResult resp)
esSearch iname body scrollReq = do
  ElasticEffect env <- getStaticRep
  -- unsafeEff_ $ BH.runBH env $ BHR.search iname (trace (show $ encode body) body) scrollReq
  unsafeEff_ $ BH.runBH env $ BHR.search iname body scrollReq

esAdvance :: (ElasticEffect :> es, FromJSON resp) => BH.ScrollId -> Eff es (BH.SearchResult resp)
esAdvance scroll = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BHR.advance scroll

esGetDocument :: ElasticEffect :> es => BH.IndexName -> BH.DocId -> Eff es (HTTP.Response LByteString)
esGetDocument iname doc = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.getDocument iname doc

esCountByIndex :: ElasticEffect :> es => BH.IndexName -> BH.CountQuery -> Eff es (Either BH.EsError BH.CountResponse)
esCountByIndex iname q = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.countByIndex iname q

esSearchHit :: ElasticEffect :> es => ToJSON body => BH.IndexName -> body -> Eff es [Json.Value]
esSearchHit iname body = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BHR.searchHit iname body

esScanSearch :: ElasticEffect :> es => FromJSON body => BH.IndexName -> BH.Search -> Eff es [BH.Hit body]
esScanSearch iname search = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.scanSearch iname search

esDeleteByQuery :: ElasticEffect :> es => BH.IndexName -> BH.Query -> Eff es BH.Reply
esDeleteByQuery iname q = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.deleteByQuery iname q

esCreateIndex :: ElasticEffect :> es => BH.IndexSettings -> BH.IndexName -> Eff es ()
esCreateIndex is iname = do
  ElasticEffect env <- getStaticRep
  -- TODO: check for error
  unsafeEff_ $ void $ BH.runBH env $ BH.createIndex is iname

esIndexDocument :: ToJSON body => ElasticEffect :> es => BH.IndexName -> BH.IndexDocumentSettings -> body -> BH.DocId -> Eff es (HTTP.Response LByteString)
esIndexDocument indexName docSettings body docId = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.indexDocument indexName docSettings body docId

esPutMapping :: ElasticEffect :> es => ToJSON mapping => BH.IndexName -> mapping -> Eff es ()
esPutMapping iname mapping = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ void $ BH.runBH env $ BH.putMapping iname mapping

esIndexExists :: ElasticEffect :> es => BH.IndexName -> Eff es Bool
esIndexExists iname = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.indexExists iname

esDeleteIndex :: ElasticEffect :> es => BH.IndexName -> Eff es (HTTP.Response LByteString)
esDeleteIndex iname = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.deleteIndex iname

esSettings :: ElasticEffect :> es => ToJSON body => BH.IndexName -> body -> Eff es ()
esSettings iname body = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BHR.settings iname body

esRefreshIndex :: ElasticEffect :> es => BH.IndexName -> Eff es (HTTP.Response LByteString)
esRefreshIndex iname = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.refreshIndex iname

esDocumentExists :: ElasticEffect :> es => BH.IndexName -> BH.DocId -> Eff es Bool
esDocumentExists iname doc = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.documentExists iname doc

esBulk :: ElasticEffect :> es => V.Vector BulkOperation -> Eff es BH.Reply
esBulk ops = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.bulk ops

esUpdateDocument :: ElasticEffect :> es => ToJSON a => BH.IndexName -> BH.IndexDocumentSettings -> a -> DocId -> Eff es BH.Reply
esUpdateDocument iname ids body doc = do
  ElasticEffect env <- getStaticRep
  unsafeEff_ $ BH.runBH env $ BH.updateDocument iname ids body doc

-- Legacy wrappers
esSearchLegacy :: (LoggerEffect :> es, ElasticEffect :> es, FromJSON a) => BH.IndexName -> BH.Search -> Eff es (BH.SearchResult a)
esSearchLegacy indexName search = do
  ElasticEffect env <- getStaticRep
  (rawResp, resp) <- unsafeEff_ $ BH.runBH env do
    -- logText . decodeUtf8 . encode $ search
    rawResp <- BH.searchByIndex indexName search
    -- logText $ show rawResp
    (\resp -> (rawResp, resp)) <$> BH.parseEsResponse rawResp
  case resp of
    Left e -> handleError e rawResp
    Right x -> pure x
 where
  handleError resp rawResp = do
    logWarn "Elastic response failed" ["status" .= BH.errorStatus resp, "message" .= BH.errorMessage resp]
    error $ "Elastic response failed: " <> show rawResp

------------------------------------------------------------------
--

-- | HTTP Effect

------------------------------------------------------------------
type HttpEnv = HTTP.Manager

data HttpEffect :: Effect
type instance DispatchOf HttpEffect = 'Static 'WithSideEffects
newtype instance StaticRep HttpEffect = HttpEffect HttpEnv

-- | 'runHttpEffect' simply add a Manager to the static rep env.
runHttpEffect :: IOE :> es => Eff (HttpEffect : es) a -> Eff es a
runHttpEffect action = do
  manager <- liftIO Monocle.Client.mkManager
  runHttpEffectWithManager manager action

runHttpEffectWithManager :: IOE :> es => HTTP.Manager -> Eff (HttpEffect : es) a -> Eff es a
runHttpEffectWithManager manager = evalStaticRep (HttpEffect manager)

-- | 'httpRequest' catches http exceptions
httpRequest ::
  HttpEffect :> es =>
  HTTP.Request ->
  Eff es (HTTP.Response LByteString)
httpRequest request = do
  HttpEffect manager <- getStaticRep
  unsafeEff_ $ HTTP.httpLbs request manager

-------------------------------------------------------------------------------
-- A network retry system

retryLimit :: Int
retryLimit = 7

-- | Retry HTTP network action, doubling backoff each time
httpRetry :: (HasCallStack, PrometheusEffect :> es, Retry :> es, LoggerEffect :> es) => Text -> Eff es a -> Eff es a
httpRetry urlLabel baseAction = Retry.recovering policy [httpHandler] (const action)
 where
  modName = case getCallStack callStack of
    ((_, srcLoc) : _) -> from (srcLocModule srcLoc)
    _ -> "N/C"
  label = (modName, urlLabel)

  backoff = 500000 -- 500ms
  policy = Retry.exponentialBackoff backoff <> Retry.limitRetries retryLimit
  action = do
    res <- baseAction
    promIncrCounter httpRequestCounter label
    pure res
  httpHandler (RetryStatus num _ _) = Handler $ \case
    HttpExceptionRequest req ctx -> do
      let url = decodeUtf8 @Text $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
          arg = decodeUtf8 $ HTTP.queryString req
          loc = if num == 0 then url <> arg else url
      logWarn "network error" ["count" .= num, "limit" .= retryLimit, "loc" .= loc, "failed" .= show @Text ctx]
      promIncrCounter httpFailureCounter label
      pure True
    InvalidUrlException _ _ -> pure False

------------------------------------------------------------------
--

-- | Demonstrate Servant.Handler implemented with Eff

------------------------------------------------------------------

type TestApi =
  "route1" Servant.:> Get '[Servant.JSON] Natural
    :<|> "route2" Servant.:> Get '[Servant.JSON] Natural

type ApiEffects' es = (IOE :> es, LoggerEffect :> es)

-- | serverEff is the effectful implementation of the TestAPI
serverEff' :: forall es. ApiEffects' es => Servant.ServerT TestApi (Eff es)
serverEff' = route1Handler Servant.:<|> route1Handler
 where
  route1Handler :: Eff es Natural
  route1Handler = do
    logInfo "Handling route" []
    pure 42

-- | liftServer convert the effectful implementation to the Handler context.
-- It is necessary to pass each effect environment so that the effects can be interpret for each request.
liftServer :: forall es. ApiEffects' es => EffStatic.Env es -> Servant.ServerT TestApi Servant.Handler
liftServer es = Servant.hoistServer (Proxy @TestApi) interpretServer serverEff'
 where
  interpretServer :: Eff es a -> Servant.Handler a
  interpretServer action = do
    liftIO do
      es' <- EffStatic.cloneEnv es
      unEff action es'

demo, demoServant, demoTest, demoCrawler :: IO ()
demo = pure ()
demoTest = defaultMain testTree
demoServant =
  runEff $ runLoggerEffect do
    unsafeEff $ \es ->
      Warp.run 8080 $ Servant.serve (Proxy @TestApi) $ liftServer es
demoCrawler = runEff $ runLoggerEffect $ runHttpEffect crawlerDemo

type CrawlerEffect' es = (IOE :> es, HttpEffect :> es, LoggerEffect :> es)

crawlerDemo :: CrawlerEffect' es => Eff es ()
crawlerDemo = withContext ("crawler" .= ("crawler-name" :: Text)) do
  logInfo "Starting crawler" []
  res <- httpRequest =<< HTTP.parseUrlThrow "http://localhost"
  logInfo ("Got: " <> show res) []

demoMultiEffects :: IO ()
demoMultiEffects = do
  runEff $ E.runFailIO $ runLoggerEffect $ runLoggerEffect $ do
    loggerDemo
    subDemo

loggerDemo :: LoggerEffect :> es => Eff es ()
loggerDemo = logInfo "Hello effectful" []

subDemo :: E.Fail :> es => Eff es ()
subDemo = fail "Toto"
