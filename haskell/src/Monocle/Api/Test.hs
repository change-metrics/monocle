module Monocle.Api.Test (mkAppEnv, withTestApi) where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Monocle.Api
import Monocle.Backend.Index qualified as I
import Monocle.Client
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Prelude
import Network.HTTP.Mock (withMockedManager)
import Network.Wai

-- Create the AppEnv, necesary to create the monocle api Wai Application
mkAppEnv :: Config.Index -> IO AppEnv
mkAppEnv workspace = do
  bhEnv <- mkEnv'
  let glLogger _ = pure ()
      config' = Config.Config Nothing [workspace]
      ws = Config.mkWorkspaceStatus config'
  wsRef <- newMVar $ fmap (const Config.Ready) ws
  let config = pure (Config.ConfigStatus False config' wsRef)
      aEnv = Env {..}
  pure $ AppEnv {..}

withTestApi :: IO AppEnv -> (Logger -> MonocleClient -> Assertion) -> IO ()
withTestApi appEnv' testCb = bracket appEnv' cleanIndex runTest
  where
    -- Using a mockedManager, run the Api behind a MonocleClient for the tests
    runTest :: AppEnv -> Assertion
    runTest appEnv = do
      conf <- Config.csConfig <$> config appEnv
      let indexes = Config.getWorkspaces conf
      traverse_
        (\index -> runQueryM' (bhEnv $ aEnv appEnv) index I.ensureIndex)
        indexes
      withMockedManager
        (dropVersionPath $ app appEnv)
        (\manager -> withLogger $ \logger -> withClient "http://localhost" (Just manager) (testCb logger))
    dropVersionPath app' req = do
      app'
        ( req
            { rawPathInfo = B.drop (B.length "/api/2/") (rawPathInfo req),
              pathInfo = drop 2 (pathInfo req)
            }
        )
    -- Remove the index
    cleanIndex :: AppEnv -> IO ()
    cleanIndex appEnv = do
      conf <- Config.csConfig <$> config appEnv
      let indexes = Config.getWorkspaces conf
      traverse_
        (\index -> runQueryM' (bhEnv $ aEnv appEnv) index I.removeIndex)
        indexes
