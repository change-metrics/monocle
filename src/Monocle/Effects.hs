{-# LANGUAGE TypeFamilies #-}

-- | This module demonstrates how the static reader based effect
-- provided by the effectful library can be used to replace the
-- current mtl style solution.
--
-- The goals are:
--
-- - Remove the `instance HasLogger Api` and `instance HasLogger Lentille` boilerplate.
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
-- If approved, then the inidivual effect should be sperated in multiple modules for
-- better re-usability.
module Monocle.Effects where

import Control.Retry (RetryPolicyM, RetryStatus (..))
import Control.Retry qualified as Retry

import Data.Aeson (Series, pairs)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Monocle.Prelude hiding (Reader, ask, local)
import System.Log.FastLogger qualified as FastLogger

import Monocle.Client qualified
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Client qualified as HTTP

import Effectful
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep)
import Effectful.Dispatch.Static.Primitive (Env, cloneEnv)
import Effectful.Reader.Static as Eff

import Monocle.Logging hiding (logInfo, withContext)

import Monocle.Config (ConfigStatus)
import Monocle.Config qualified

import Control.Exception (finally)
import GHC.IO.Handle (hClose)
import System.Directory
import System.Posix.Temp (mkstemp)
import Test.Tasty
import Test.Tasty.HUnit

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Get, (:<|>))
import Servant qualified

type TestApi =
  "route1" Servant.:> Get '[Servant.JSON] Natural
    :<|> "route2" Servant.:> Get '[Servant.JSON] Natural

-- | serverEff is the effectful implementation
serverEff :: forall es. [IOE, LoggerEffect] :>> es => Servant.ServerT TestApi (Eff es)
serverEff = route1Handler Servant.:<|> route1Handler
 where
  route1Handler :: Eff es Natural
  route1Handler = do
    logInfo "Handling route" []
    pure 42

-- | liftServer convert the effectful implementation to the Handler context
liftServer :: Logger -> Servant.ServerT TestApi Servant.Handler
liftServer logger = Servant.hoistServer (Proxy @TestApi) interpretServer serverEff
 where
  interpretServer :: Eff '[LoggerEffect, IOE] a -> Servant.Handler a
  interpretServer =
    liftIO . runEff . runLoggerEffect' logger

demo :: IO ()
demo = do
  withLogger \logger -> do
    Warp.run 8080 $ Servant.serve (Proxy @TestApi) $ liftServer logger

-- defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Monocle.Effects"
    [ testCase "LoggerEffect" do
        runEff $ runLoggerEffect do
          logInfo "logInfo prints!" []
    , testCase "MonoConfig" do
        (path, fd) <- mkstemp "/tmp/monoconfig-test"
        hClose fd
        runEff (runMonoConfig path (testMonoConfig path)) `finally` removeFile path
    ]
 where
  testEff a b = liftIO (a @?= b)
  testMonoConfig :: [MonoConfigEffect, IOE] :>> es => FilePath -> Eff es ()
  testMonoConfig fp = do
    -- Setup the test config
    let getNames c = Monocle.Config.getWorkspaceName <$> Monocle.Config.getWorkspaces (Monocle.Config.csConfig c)
    liftIO do writeFile fp "workspaces: []"
    reloadConfig <- mkReloadConfig

    -- initial load
    do
      config <- reloadConfig
      Monocle.Config.csReloaded config `testEff` False
      getNames config `testEff` []

    -- test reload works
    do
      liftIO do writeFile fp "workspaces:\n- name: test\n  crawlers: []"
      config <- reloadConfig
      Monocle.Config.csReloaded config `testEff` True
      getNames config `testEff` ["test"]

    -- make sure reload is avoided when the file doesn't change
    do
      config <- reloadConfig
      Monocle.Config.csReloaded config `testEff` False

------------------------------------------------------------------
--

-- | Config effect to load and reload the local config

------------------------------------------------------------------

-- | The effect environment
type MonoConfigEnv = FilePath

-- | The effect definition using static rep.
data MonoConfigEffect :: Effect

type instance DispatchOf MonoConfigEffect = 'Static WithSideEffects
newtype instance StaticRep MonoConfigEffect = MonoConfigEffect MonoConfigEnv

-- | Run the effect (e.g. removes it from the list)
runMonoConfig :: IOE :> es => FilePath -> Eff (MonoConfigEffect : es) a -> Eff es a
runMonoConfig fp = evalStaticRep (MonoConfigEffect fp)

-- | The lifted version of Monocle.Config.reloadConfig
mkReloadConfig :: MonoConfigEffect :> es => Eff es (Eff es ConfigStatus)
mkReloadConfig = do
  MonoConfigEffect fp <- getStaticRep
  (mkReload :: IO ConfigStatus) <- unsafeEff_ (Monocle.Config.reloadConfig fp)
  pure $ unsafeEff_ mkReload

-- runEff $ runLoggerEffect $ runHttpEffect $ crawler

crawler :: (LoggerEffect :> es, HttpEffect :> es) => Eff es ()
crawler = withContext ("crawler" .= ("crawler-name" :: Text)) do
  logInfo "Starting crawler" []
  void $ httpRequest =<< HTTP.parseUrlThrow "http://localhost"

-- | ---------------------------------------------------------------------
-- | HTTP effect
data HttpContext = HttpContext
  { httpManager :: HTTP.Manager
  }

data HttpEffect :: Effect
type instance DispatchOf HttpEffect = Static WithSideEffects
newtype instance StaticRep HttpEffect = HttpEffect HttpContext

runHttpEffect :: IOE :> es => Eff (HttpEffect : es) a -> Eff es a
runHttpEffect action = do
  ctx <- HttpContext <$> liftIO Monocle.Client.mkManager
  evalStaticRep (HttpEffect ctx) action

httpRequest :: (LoggerEffect :> es, HttpEffect :> es) => HTTP.Request -> Eff es (HTTP.Response LByteString)
httpRequest req = do
  HttpEffect (HttpContext manager) <- getStaticRep
  resp <- unsafeEff $ \env ->
    Retry.recovering policy [httpHandler env] (const $ HTTP.httpLbs req manager)
  logInfo "Got resp" ["status" .= show @Text resp]
  pure resp
 where
  retryLimit :: Int
  retryLimit = 2

  backoff = 500000 -- 500ms
  policy = Retry.exponentialBackoff backoff <> Retry.limitRetries retryLimit
  httpHandler env (RetryStatus num _ _) = Handler $ \case
    HttpExceptionRequest req ctx -> do
      let url = decodeUtf8 @Text $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
          arg = decodeUtf8 $ HTTP.queryString req
          loc = if num == 0 then url <> arg else url
      flip unEff env (logInfo "network error" ["count" .= num, "limit" .= retryLimit, "loc" .= loc, "failed" .= show @Text ctx])
      pure True
    InvalidUrlException _ _ -> pure False

------------------------------------------------------------------
--

-- | Logging effect based on the current Monocle.Logging.HasEffect

------------------------------------------------------------------

type LoggerEffect = Reader Logger

runLoggerEffect :: IOE :> es => Eff (LoggerEffect : es) a -> Eff es a
runLoggerEffect action =
  -- `withEffToIO` and `unInIO` enables calling IO function like: `(Logger -> IO a) -> IO a`.
  withEffToIO $ \runInIO ->
    withLogger \logger ->
      runInIO $ Eff.runReader logger action

runLoggerEffect' :: Logger -> Eff (LoggerEffect : es) a -> Eff es a
runLoggerEffect' logger = Eff.runReader logger

withContext :: LoggerEffect :> es => Series -> Eff es a -> Eff es a
withContext ctx = local $ \(Logger prevCtx logger) -> Logger (ctx <> prevCtx) logger

doLog :: LoggerEffect :> es => LogLevel -> ByteString -> Text -> [Series] -> Eff es ()
doLog lvl loc msg attrs = do
  Logger ctx logger <- ask
  let body :: ByteString
      body = case from . encodingToLazyByteString . pairs . mappend ctx . mconcat $ attrs of
        "{}" -> mempty
        x -> " " <> x
  -- `unsafeEff_` is equivalent to `liftIO`
  unsafeEff_ $ logger (\time -> FastLogger.toLogStr $ time <> msgText <> body <> "\n")
 where
  msgText :: ByteString
  msgText = from lvl <> loc <> ": " <> encodeUtf8 msg

logInfo :: (HasCallStack, LoggerEffect :> es) => Text -> [Series] -> Eff es ()
logInfo = doLog LogInfo getLocName
