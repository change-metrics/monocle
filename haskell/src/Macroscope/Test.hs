-- | Tests for the macroscope process
module Macroscope.Test where

import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Macroscope.Worker as Macroscope
import Monocle.Api
import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Index as I
import qualified Monocle.Backend.Provisioner
import qualified Monocle.Backend.Queries as Q
import Monocle.Client
import Monocle.Env
import Monocle.Prelude
import Network.HTTP.Mock (withMockedManager)
import Network.Wai
import qualified Streaming.Prelude as Streaming
import Test.Tasty
import Test.Tasty.HUnit

withTestApi :: Config.Index -> (MonocleClient -> Assertion) -> IO ()
withTestApi config' testCb = bracket mkAppEnv runTest cleanIndex
  where
    -- Create the AppEnv, necesary to create the monocle api Wai Application
    mkAppEnv :: IO AppEnv
    mkAppEnv = do
      bhEnv <- mkEnv'
      let glLogger _ = pure ()
          config = pure [config']
          aEnv = Env {..}
      pure $ AppEnv {..}
    -- Using a mockedManager, run the Api behind a MonocleClient for the tests
    runTest :: AppEnv -> Assertion
    runTest appEnv = do
      runQueryM' (bhEnv $ aEnv appEnv) config' $ I.ensureIndex
      withMockedManager
        (dropVersionPath $ app appEnv)
        (\manager -> withClient "http://localhost" (Just manager) testCb)
    dropVersionPath app' req = do
      app'
        ( req
            { rawPathInfo = B.drop (B.length "/api/2/") (rawPathInfo req),
              pathInfo = drop 2 (pathInfo req)
            }
        )
    -- Remove the index
    cleanIndex :: AppEnv -> IO ()
    cleanIndex appEnv = runQueryM' (bhEnv $ aEnv appEnv) config' $ I.removeIndex

testTaskDataMacroscope :: Assertion
testTaskDataMacroscope = withTestApi fakeConfig $ \client -> do
  -- Start the macroscope with a fake stream
  now <- toMonocleTime <$> getCurrentTime
  td <- Monocle.Backend.Provisioner.generateNonDeterministic Monocle.Backend.Provisioner.fakeTaskData
  let stream _ = Streaming.each [td]
  Macroscope.runStream client now apiKey indexName crawlerName (Macroscope.TaskDatas stream)
  -- Check task data got indexed
  count <- testQueryM fakeConfig $ withQuery taskDataQuery $ Streaming.length_ Q.scanSearchId
  assertEqual "Task data got indexed by macroscope" count 1
  where
    taskDataQuery = mkQuery [mkTerm "tasks_data.crawler_name" (toText crawlerName)]
    fakeConfig =
      (mkConfig (toText indexName))
        { Config.crawlers_api_key = Just (toText apiKey),
          Config.crawlers =
            [ let name = toText crawlerName
                  update_since = "2000-01-01"
                  provider = Config.TaskDataProvider
               in Config.Crawler {..}
            ]
        }

    apiKey = "secret"
    indexName = "test-macroscope"
    crawlerName = "testy"

monocleMacroscopeTests :: TestTree
monocleMacroscopeTests = testGroup "Macroscope" [testCase "TaskData stream" testTaskDataMacroscope]
