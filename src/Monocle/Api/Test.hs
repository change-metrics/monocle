{-# LANGUAGE OverloadedRecordDot #-}

module Monocle.Api.Test (mkAppEnv, withTestApi) where

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

import Monocle.Effects

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

runAppEnv :: [ElasticEffect, LoggerEffect, IOE] :>> es => AppEnv -> Eff es a -> IO a
runAppEnv appEnv action =
  runEff
    . runLoggerEffect
    . runElasticEffect (appEnv.aEnv.bhEnv)
    $ action

withTestApi :: IO AppEnv -> (Logger -> MonocleClient -> Assertion) -> IO ()
withTestApi appEnv' testCb = bracket appEnv' cleanIndex runTest
 where
  -- Using a mockedManager, run the Api behind a MonocleClient for the tests
  runTest :: AppEnv -> Assertion
  runTest appEnv = runAppEnv appEnv $ do
    conf <- Config.csConfig <$> liftIO (appEnv.config)
    let indexes = Config.getWorkspaces conf
    traverse_
      (\index -> runEmptyMonoQuery index I.ensureIndex)
      indexes
  {- TODO: migrate to effectful
  withMockedManager
    (dropVersionPath $ app appEnv)
    (\manager -> withLogger $ \logger -> withClient "http://localhost" (Just manager) (testCb logger))
  -}
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
      (\index -> runEmptyMonoQuery index index I.removeIndex)
      indexes
