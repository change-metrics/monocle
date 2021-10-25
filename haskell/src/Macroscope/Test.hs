-- | Tests for the macroscope process
module Macroscope.Test where

import Control.Exception (bracket)
import qualified Data.ByteString as B
import Lentille (runLentilleM)
import qualified Macroscope.Worker as Macroscope
import Monocle.Api
import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Documents as D
import qualified Monocle.Backend.Index as I
import qualified Monocle.Backend.Provisioner
import qualified Monocle.Backend.Queries as Q
import qualified Monocle.Backend.Test as BT (fakeChange, fakeDate, fakeDateAlt)
import Monocle.Client
import Monocle.Env
import Monocle.Prelude
import Network.HTTP.Mock (withMockedManager)
import Network.Wai
import qualified Streaming.Prelude as Streaming
import Test.Tasty
import Test.Tasty.HUnit

-- Create the AppEnv, necesary to create the monocle api Wai Application
mkAppEnv :: Config.Index -> IO AppEnv
mkAppEnv conf = do
  bhEnv <- mkEnv'
  let glLogger _ = pure ()
      config = pure [conf]
      aEnv = Env {..}
  pure $ AppEnv {..}

withTestApi :: Config.Index -> (MonocleClient -> Assertion) -> IO ()
withTestApi config' testCb = bracket (mkAppEnv config') cleanIndex runTest
  where
    -- Using a mockedManager, run the Api behind a MonocleClient for the tests
    runTest :: AppEnv -> Assertion
    runTest appEnv = do
      runQueryM' (bhEnv $ aEnv appEnv) config' I.ensureIndex
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
    cleanIndex appEnv = do
      runQueryM' (bhEnv $ aEnv appEnv) config' I.removeIndex

testCrawlingPoint :: Assertion
testCrawlingPoint = do
  appEnv <- mkAppEnv fakeConfig
  void $ runQueryM' (bhEnv $ aEnv appEnv) fakeConfig I.ensureIndexSetup
  let fakeChange1 =
        BT.fakeChange
          { D.echangeId = "efake1",
            D.echangeUpdatedAt = BT.fakeDate,
            D.echangeRepositoryFullname = "opendev/neutron"
          }
      fakeChange2 = fakeChange1 {D.echangeId = "efake2", D.echangeUpdatedAt = BT.fakeDateAlt}
  void $ runQueryM' (bhEnv $ aEnv appEnv) fakeConfig $ I.indexChanges [fakeChange1, fakeChange2]
  withTestApi fakeConfig $ \client -> do
    now <- toMonocleTime <$> getCurrentTime
    let stream date name
          | date == BT.fakeDateAlt && name == "opendev/neutron" = pure mempty
          | otherwise = error "Bad crawling point"
    void $ runLentilleM $ Macroscope.runStream client now apiKey indexName crawlerName (Macroscope.Changes stream)
    assertEqual "Fetched at expected crawling point" True True
  where
    fakeConfig =
      (mkConfig (toText indexName))
        { Config.crawlers_api_key = Just (toText apiKey),
          Config.crawlers =
            [ let name = toText crawlerName
                  update_since = "2000-01-01"
                  provider =
                    Config.GerritProvider
                      ( Config.Gerrit
                          { gerrit_login = Nothing,
                            gerrit_password = Nothing,
                            gerrit_prefix = Nothing,
                            gerrit_repositories = Just ["opendev/neutron"],
                            gerrit_url = "https://fake.url"
                          }
                      )
               in Config.Crawler {..}
            ]
        }
    apiKey = "secret"
    indexName = "test-macroscope"
    crawlerName = "testy"

testTaskDataMacroscope :: Assertion
testTaskDataMacroscope = withTestApi fakeConfig $ \client -> do
  -- Start the macroscope with a fake stream
  now <- toMonocleTime <$> getCurrentTime
  td <- Monocle.Backend.Provisioner.generateNonDeterministic Monocle.Backend.Provisioner.fakeTaskData
  let stream _untilDate _project = Streaming.each [td]
  void $ runLentilleM $ Macroscope.runStream client now apiKey indexName crawlerName (Macroscope.TaskDatas stream)
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
                  provider =
                    Config.BugzillaProvider $
                      Config.Bugzilla
                        { bugzilla_products = Just ["fake_product"],
                          bugzilla_token = Nothing,
                          bugzilla_url = ""
                        }
               in Config.Crawler {..}
            ]
        }

    apiKey = "secret"
    indexName = "test-macroscope"
    crawlerName = "testy"

monocleMacroscopeTests :: TestTree
monocleMacroscopeTests =
  testGroup
    "Macroscope"
    [ testCase "TaskData stream" testTaskDataMacroscope,
      testCase "Change stream (crawling point)" testCrawlingPoint
    ]
