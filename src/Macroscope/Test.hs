-- | Tests for the macroscope process
module Macroscope.Test where

import Effectful.Env
import Effectful.Prometheus
import Effectful.Reader.Static qualified as E
import Lentille
import Lentille.Bugzilla (BZEffect)
import Lentille.Gerrit (GerritEffect)
import Macroscope.Main qualified as Macroscope
import Macroscope.Worker qualified as Macroscope
import Monocle.Api.Test (mkAppEnv, runAppEnv, withTestApi)
import Monocle.Backend.Documents qualified as D
import Monocle.Backend.Index qualified as I
import Monocle.Backend.Provisioner qualified
import Monocle.Backend.Queries qualified as Q
import Monocle.Backend.Test (withTenantConfig)
import Monocle.Backend.Test qualified as BT (fakeChange, fakeDate, fakeDateAlt)
import Monocle.Client
import Monocle.Config qualified as Config
import Monocle.Effects
import Monocle.Entity (CrawlerName (..))
import Monocle.Env
import Monocle.Prelude
import Streaming.Prelude qualified as Streaming
import Test.Tasty
import Test.Tasty.HUnit

runLentilleM :: MonocleClient -> Eff [E.Reader CrawlerEnv, MonoClientEffect, LoggerEffect, GerritEffect, BZEffect, TimeEffect, RetryEffect, HttpEffect, PrometheusEffect, EnvEffect, Fail, Concurrent, IOE] a -> IO a
runLentilleM client action = do
  env <- CrawlerEnv client <$> newIORef False
  runEff . Macroscope.runMacroEffects . runLoggerEffect . runMonoClient client . E.runReader env $ action

testCrawlingPoint :: Assertion
testCrawlingPoint = do
  appEnv <- mkAppEnv fakeConfig
  runAppEnv appEnv $ runEmptyQueryM fakeConfig do
    I.ensureIndexSetup
    let fakeChange1 =
          BT.fakeChange
            { D.echangeId = "efake1"
            , D.echangeUpdatedAt = BT.fakeDate
            , D.echangeRepositoryFullname = "opendev/neutron"
            }
        fakeChange2 = fakeChange1 {D.echangeId = "efake2", D.echangeUpdatedAt = BT.fakeDateAlt}
    I.indexChanges [fakeChange1, fakeChange2]
  withTestApi (mkAppEnv fakeConfig) $ \client -> do
    let stream date name
          | date == BT.fakeDateAlt && name == "opendev/neutron" = pure mempty
          | otherwise = error "Bad crawling point"
    void $ runLentilleM client $ Macroscope.runStream apiKey indexName (CrawlerName crawlerName) (Macroscope.Changes stream)
    assertEqual "Fetched at expected crawling point" True True
 where
  fakeConfig =
    (mkConfig (from indexName))
      { Config.crawlers_api_key = Just (from apiKey)
      , Config.crawlers =
          [ let name = from crawlerName
                update_since = "2000-01-01"
                provider =
                  Config.GerritProvider
                    ( Config.Gerrit
                        { gerrit_login = Nothing
                        , gerrit_password = Nothing
                        , gerrit_prefix = Nothing
                        , gerrit_repositories = Just ["opendev/neutron"]
                        , gerrit_url = "https://fake.url"
                        }
                    )
             in Config.Crawler {..}
          ]
      }
  apiKey = "secret"
  indexName = "test-macroscope"
  crawlerName = "testy"

testTaskDataMacroscope :: Assertion
testTaskDataMacroscope = withTestApi appEnv $ \client -> testAction client
 where
  testAction :: MonocleClient -> IO ()
  testAction client = do
    -- Start the macroscope with a fake stream
    td <- Monocle.Backend.Provisioner.generateNonDeterministic Monocle.Backend.Provisioner.fakeTaskData
    let stream _untilDate project
          | project == "fake_product" = Streaming.each [td]
          | otherwise = error $ "Unexpected product entity: " <> show project
    void $ runLentilleM client $ Macroscope.runStream apiKey indexName (CrawlerName crawlerName) (Macroscope.TaskDatas stream)
    -- Check task data got indexed
    withTenantConfig fakeConfig do
      count <- withQuery taskDataQuery $ Streaming.length_ Q.scanSearchId
      liftIO (assertEqual "Task data got indexed by macroscope" count 1)

  appEnv = mkAppEnv fakeConfig
  taskDataQuery = mkQuery [mkTerm "tasks_data.crawler_name" (from crawlerName)]
  fakeConfig =
    (mkConfig (from indexName))
      { Config.crawlers_api_key = Just (from apiKey)
      , Config.crawlers =
          [ let name = from crawlerName
                update_since = "2000-01-01"
                provider =
                  Config.BugzillaProvider $
                    Config.Bugzilla
                      { bugzilla_products = Just ["fake_product"]
                      , bugzilla_token = Nothing
                      , bugzilla_url = ""
                      }
             in Config.Crawler {..}
          ]
      }

  apiKey = "secret"
  indexName = "test-macroscope"
  crawlerName = "testy"

testRunCrawlers :: Assertion
testRunCrawlers = do
  -- Return False then True
  reloadRef <- newIORef False
  let isReload = do
        reload <- readIORef reloadRef
        writeIORef reloadRef True
        pure reload

  logs <- newTVarIO []

  let tell' :: MonadIO m => Text -> m ()
      tell' x = atomically $ modifyTVar' logs (x :)

  let streams =
        [ ("gitlab", fromList [tell' "gl1", tell' "gl2"])
        , ("gerrit", fromList [tell' "gr"])
        ]

      -- We expect both streams to run twice:
      --   0ms: watcher read the reloadRef (False)
      --   0ms: gitlab starts
      --  10ms: gerrit starts
      --  25ms: gitlab loop
      --  35ms: gerrit loop
      --  70ms: watcher read the reloadRef (True) and wait for crawlers
      expected = ["gl1", "gl2", "gr", "gl1", "gl2", "gr"]

  withClient "http://localhost" Nothing $ \client ->
    runLentilleM client $
      Macroscope.runCrawlers' 10_000 25_000 70_000 isReload streams

  got <- reverse <$> readTVarIO logs
  assertEqual "Stream ran" expected got

testGetStream :: Assertion
testGetStream = do
  setEnv "CRAWLERS_API_KEY" "secret"
  setEnv "GITLAB_TOKEN" "42"
  setEnv "OTHER_TOKEN" "43"
  withClient "http://localhost" Nothing $ \client -> runLentilleM client do
    (streams, clients) <- runStateT (traverse Macroscope.getCrawler (Macroscope.getCrawlers conf)) (Macroscope.Clients mempty mempty mempty)
    assertEqual' "Two streams created" 3 (length streams)
    assertEqual' "Only two gitlab clients created" 2 (length $ toList $ Macroscope.clientsGraph clients)
    let expected = ["http://localhost-42 for crawler-for-org1, crawler-for-org2", "http://localhost-43 for crawler-for-org3"]
    assertEqual' "Stream group named" expected (map fst $ Macroscope.mkStreamsActions (catMaybes streams))
 where
  conf =
    [ (Config.mkTenant "test-stream")
        { Config.crawlers = [gl "org1" "GITLAB_TOKEN", gl "org2" "GITLAB_TOKEN", gl "org3" "OTHER_TOKEN"]
        , Config.crawlers_api_key = Just "CRAWLERS_API_KEY"
        }
    ]
  gl gitlab_organization token =
    let gitlab_url = Just "http://localhost"
        gitlab_repositories = Nothing
        gitlab_token = Just token
        provider = Config.GitlabProvider Config.Gitlab {..}
        name = "crawler-for-" <> gitlab_organization
        update_since = "2021-01-01"
     in Config.Crawler {..}

monocleMacroscopeTests :: TestTree
monocleMacroscopeTests =
  testGroup
    "Macroscope"
    [ testCase "GetStream reuse client" testGetStream
    , testCase "RunCrawlers" testRunCrawlers
    , testCase "TaskData stream" testTaskDataMacroscope
    , testCase "Change stream (crawling point)" testCrawlingPoint
    ]
