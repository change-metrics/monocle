{-# LANGUAGE OverloadedRecordDot #-}

module Monocle.Api.Test (mkAppEnv, withTestApi, runAppEnv) where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Monocle.Backend.Index qualified as I
import Monocle.Client
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Main
import Monocle.Prelude
import Network.HTTP.Mock (withMockedManager)
import Network.Wai
import Servant.Auth.Server (
  defaultCookieSettings,
  defaultJWTSettings,
  generateKey,
 )

import Database.Bloodhound qualified as BH
import Effectful.Error.Static qualified as E
import Effectful.Fail qualified as E
import Effectful.Reader.Static qualified as E
import Monocle.Effects
import Servant (Context (..), ServerError)

import Effectful.Concurrent.MVar qualified as E
import Effectful.Servant qualified

-- Create the AppEnv, necesary to create the monocle api Wai Application
mkAppEnv :: Config.Index -> IO AppEnv
mkAppEnv workspace = do
  bhEnv <- mkEnv'
  let config' = Config.Config Nothing Nothing [workspace]
      ws = Config.mkWorkspaceStatus config'
  wsRef <- newMVar $ fmap (const Config.Ready) ws
  jwk <- generateKey
  let aOIDC = OIDC Nothing (defaultJWTSettings jwk)
      config = pure (Config.ConfigStatus False config' wsRef)
  pure $ AppEnv {..}

-- | Run the test effects list using an existing AppEnv. This is useful for legacy test written for QueryM ()
runAppEnv :: AppEnv -> Eff '[ElasticEffect, MonoConfigEffect, E.Reader AppEnv, LoggerEffect, E.Error ServerError, Retry, E.Fail, E.Concurrent, IOE] a -> IO a
runAppEnv appEnv =
  runEff
    . E.runConcurrent
    . E.runFailIO
    . runRetry
    . (fmap (fromRight (error "oops")) . E.runErrorNoCallStack)
    . runLoggerEffect
    . E.runReader appEnv
    . runMonoConfigFromEnv (appEnv.config)
    . runElasticEffect (appEnv.bhEnv :: BH.BHEnv)

-- | Run the api effects and provide a http client to the callback for api testing.
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
        let app = Effectful.Servant.hoistEff @RootAPI es cfg rootServer
            withManager manager = do
              withClient "http://localhost" (Just manager) $ \client -> do
                testCb client
         in withMockedManager (dropVersionPath app) withManager

  -- TODO: check if dropVersionPath is still necessary
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
    conf <- Config.csConfig <$> getReloadConfig
    let indexes = Config.getWorkspaces conf
    traverse_
      (\index -> runEmptyQueryM index I.removeIndex)
      indexes
