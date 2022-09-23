{-# LANGUAGE OverloadedRecordDot #-}

module Monocle.Api.Test (mkAppEnv, withTestApi, runAppEnv) where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Monocle.Backend.Index qualified as I
import Monocle.Client
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Main
import Monocle.Prelude
import Network.HTTP.Mock (withMockedManager)
import Network.Wai
import Servant.Auth.Server (defaultJWTSettings, generateKey)

import Database.Bloodhound qualified as BH
import Effectful.Error.Static qualified as E
import Effectful.Fail qualified as E
import Effectful.Reader.Static qualified as E
import Monocle.Effects
import Servant (Context (..), ServerError)
import Servant.Auth.Server (defaultCookieSettings)

import Effectful.Concurrent.MVar qualified as E

-- Create the AppEnv, necesary to create the monocle api Wai Application
mkAppEnv :: Config.Index -> IO AppEnv
mkAppEnv workspace = withLogger \glLogger -> do
  bhEnv <- mkEnv'
  let config' = Config.Config Nothing Nothing [workspace]
      ws = Config.mkWorkspaceStatus config'
  wsRef <- newMVar $ fmap (const Config.Ready) ws
  jwk <- generateKey
  let aOIDC = OIDC Nothing (defaultJWTSettings jwk)
      config = pure (Config.ConfigStatus False config' wsRef)
      aEnv = Env {..}
  pure $ AppEnv {..}

--  Note: when running Effect, the order is set
runAppEnv :: AppEnv -> Eff (ElasticEffect : MonoConfigEffect : E.Reader AppEnv : LoggerEffect : E.Error ServerError : E.Fail : E.Concurrent : IOE : '[]) a -> IO a
runAppEnv appEnv =
  runEff
    . E.runConcurrent
    . E.runFailIO
    . (fmap (fromRight (error "oops")) . E.runErrorNoCallStack)
    . runLoggerEffect
    . E.runReader appEnv
    . runMonoConfigFromEnv (appEnv.config)
    . runElasticEffect (appEnv.aEnv.bhEnv :: BH.BHEnv)

withTestApi :: IO AppEnv -> (MonocleClient -> IO ()) -> Assertion
withTestApi appEnv' testCb = bracket appEnv' cleanIndex runTest
 where
  -- Using a mockedManager, run the Api behind a MonocleClient for the tests
  runTest :: AppEnv -> Assertion
  runTest appEnv = runAppEnv appEnv testSetup
   where
    -- testSetup :: TestEffects es => Eff es ()
    testSetup = do
      conf <- Config.csConfig <$> liftIO (appEnv.config)
      let indexes = Config.getWorkspaces conf
          cfg = appEnv.aOIDC.localJWTSettings :. defaultCookieSettings :. EmptyContext
      traverse_
        (\index -> runEmptyQueryM index I.ensureIndex)
        indexes
      unsafeEff $ \es ->
        let app = hoistEff @RootAPI es cfg rootServer
            withManager manager = do
              withClient "http://localhost" (Just manager) $ \client -> do
                testCb client
         in withMockedManager (dropVersionPath app) withManager

  dropVersionPath app' req = do
    app'
      ( req
          { rawPathInfo = B.drop (B.length "/api/2/") (rawPathInfo req)
          , pathInfo = drop 2 (pathInfo req)
          }
      )
  -- Remove the index
  cleanIndex :: AppEnv -> IO ()
  cleanIndex appEnv = runAppEnv appEnv $ do
    conf <- Config.csConfig <$> (liftIO appEnv.config)
    let indexes = Config.getWorkspaces conf
    traverse_
      (\index -> runEmptyQueryM index I.removeIndex)
      indexes
