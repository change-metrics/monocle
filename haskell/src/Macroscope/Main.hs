-- |
module Macroscope.Main (runMacroscope) where

import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import qualified Control.Scheduler as Scheduler (Comp (..), traverseConcurrently_)
import qualified Data.Text as T
import Lentille
import Lentille.Bugzilla (BugzillaSession, MonadBZ, getApikey, getBZData, getBugzillaSession)
import Lentille.Gerrit (MonadGerrit (..))
import qualified Lentille.Gerrit as GerritCrawler (GerritEnv (..), getChangesStream, getProjectsStream)
import Lentille.GitHub.Issues (streamLinkedIssue)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Lentille.GraphQL
import Macroscope.Worker (DocumentStream (..), runStream)
import qualified Monocle.Api.Config as Config
import Monocle.Client
import Monocle.Prelude
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Prometheus (exportMetricsAsText, register)
import Prometheus.Metric.GHC (ghcMetrics)

data CrawlerInfo = CrawlerInfo
  { cName :: Text,
    cKey :: Text,
    cCrawler :: Config.Crawler,
    cIdents :: [Config.Ident]
  }

-- | Utility function to create a flat list of crawler from the whole configuration
getCrawlers :: [Config.Index] -> [CrawlerInfo]
getCrawlers xs = do
  Config.Index {..} <- xs
  cCrawler <- crawlers
  let cKey = fromMaybe (error "unknown crawler key") crawlers_api_key
      cIdents = fromMaybe [] idents
      cName = name
  pure $ CrawlerInfo {..}

crawlerName :: Config.Crawler -> Text
crawlerName Config.Crawler {..} = name

runMonitoringServer :: Int -> IO ()
runMonitoringServer port = do
  -- Setup GHC metrics for prometheus
  void $ register ghcMetrics

  -- Start the Wai application in the background with Warp
  v <- newEmptyMVar
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar v ()) $ Warp.defaultSettings
  void $ forkIO $ Warp.runSettings settings $ app
  mLog $ Log Macroscope $ LogStartingMonitoring port

  -- Wait for the application to be ready
  takeMVar v
  where
    app req resp = case Wai.rawPathInfo req of
      "/health" -> resp $ Wai.responseLBS HTTP.status200 [] "api is running\n"
      "/metrics" -> resp . Wai.responseLBS HTTP.ok200 [] =<< exportMetricsAsText
      _anyOtherPath -> resp $ Wai.responseLBS HTTP.notFound404 [] mempty

-- | 'run' is the entrypoint of the macroscope process
-- withClient "http://localhost:8080" Nothing $ \client -> runMacroscope True "/home/user/git/github.com/change-metrics/monocle/etc/config.yaml" 30 client
runMacroscope :: Int -> FilePath -> Word32 -> MonocleClient -> IO ()
runMacroscope port confPath interval client = do
  runMonitoringServer port
  runMacroscope' confPath interval client

runMacroscope' :: FilePath -> Word32 -> MonocleClient -> IO ()
runMacroscope' confPath interval client = runLentilleM client $ do
  mLog $ Log Macroscope LogMacroStart
  config <- Config.mReloadConfig confPath
  loop config
  where
    loop config = do
      -- Reload config
      conf <- config

      -- Create all the streams
      let crawlerInfos = getCrawlers conf
      streams <- traverse getStream crawlerInfos

      -- Crawl each stream
      let traverseParallel = Scheduler.traverseConcurrently_ Scheduler.Seq
      traverseParallel runCrawler $ zip crawlerInfos streams

      -- Pause
      mLog $ Log Macroscope $ LogMacroPause interval
      mThreadDelay $ fromIntegral $ interval * 1_000_000

      -- Loop again
      loop config

type MonadMacro m = (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m, MonadReader CrawlerEnv m)

runCrawler :: MonadMacro m => (CrawlerInfo, [DocumentStream m]) -> m ()
runCrawler = safeCrawl
  where
    safeCrawl :: MonadMacro m => (CrawlerInfo, [DocumentStream m]) -> m ()
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right comp -> pure comp
        Left exc ->
          let (CrawlerInfo index _ Config.Crawler {..} _, _) = crawler
           in mLog $ Log Macroscope $ LogMacroSkipCrawler (LogCrawlerContext index name) (show exc)

    crawl :: MonadMacro m => (CrawlerInfo, [DocumentStream m]) -> m ()
    crawl (CrawlerInfo index key crawler _, docStreams) = do
      now <- toMonocleTime <$> mGetCurrentTime
      mLog $ Log Macroscope $ LogMacroStartCrawler $ LogCrawlerContext index (crawlerName crawler)

      let runner = runStream now (toLazy key) (toLazy index) (toLazy $ crawlerName crawler)

      -- TODO: handle exceptions
      traverse_ runner docStreams

getStream :: MonadMacro m => CrawlerInfo -> m [DocumentStream m]
getStream (CrawlerInfo _ _ crawler idents) = getStream'
  where
    getStream' =
      -- Create document streams
      case Config.provider crawler of
        Config.GitlabProvider Config.Gitlab {..} -> do
          -- TODO: the client may be created once for each api key
          token <- Config.mGetSecret "GITLAB_TOKEN" gitlab_token
          glClient <-
            newGraphClient
              (fromMaybe "https://gitlab.com/api/graphql" gitlab_url)
              token
          pure $
            [glOrgCrawler glClient | isNothing gitlab_repositories]
              -- Then we always index the projects
              <> [glMRCrawler glClient getIdentByAliasCB]
        Config.GerritProvider Config.Gerrit {..} -> do
          auth <- case gerrit_login of
            Just login -> do
              passwd <- Config.mGetSecret "GERRIT_PASSWORD" gerrit_password
              pure $ Just (login, passwd)
            Nothing -> pure Nothing
          gClient <- getGerritClient gerrit_url auth
          let gerritEnv = GerritCrawler.GerritEnv gClient gerrit_prefix getIdentByAliasCB
          pure $
            [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
              <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
        Config.BugzillaProvider Config.Bugzilla {..} -> do
          bzTokenT <- Config.mGetSecret "BUGZILLA_TOKEN" bugzilla_token
          bzClient <- getBugzillaSession bugzilla_url $ Just $ getApikey (unSecret bzTokenT)
          pure [bzCrawler bzClient]
        Config.GithubProvider ghCrawler -> do
          let Config.Github _ _ github_token github_url = ghCrawler
          ghToken <- Config.mGetSecret "GITHUB_TOKEN" github_token
          ghClient <- newGraphClient (fromMaybe "https://api.github.com/graphql" github_url) ghToken
          pure [ghIssuesCrawler ghClient]
        Config.GithubApplicationProvider _ -> error "Not (yet) implemented"
        Config.TaskDataProvider -> pure [] -- This is a generic crawler, not managed by the macroscope
    getIdentByAliasCB :: Text -> Maybe Text
    getIdentByAliasCB = flip Config.getIdentByAliasFromIdents idents

    glMRCrawler :: MonadGraphQLE m => GraphClient -> (Text -> Maybe Text) -> DocumentStream m
    glMRCrawler glClient cb = Changes $ streamMergeRequests glClient cb

    glOrgCrawler :: MonadGraphQLE m => GraphClient -> DocumentStream m
    glOrgCrawler glClient = Projects $ streamGroupProjects glClient

    bzCrawler :: MonadBZ m => BugzillaSession -> DocumentStream m
    bzCrawler bzSession = TaskDatas $ getBZData bzSession

    ghIssuesCrawler :: MonadGraphQLE m => GraphClient -> DocumentStream m
    ghIssuesCrawler ghClient = TaskDatas $ streamLinkedIssue ghClient

    gerritRegexProjects :: [Text] -> [Text]
    gerritRegexProjects projects = filter (T.isPrefixOf "^") projects

    gerritREProjectsCrawler :: MonadGerrit m => GerritCrawler.GerritEnv -> DocumentStream m
    gerritREProjectsCrawler gerritEnv = Projects $ GerritCrawler.getProjectsStream gerritEnv

    gerritChangesCrawler :: MonadGerrit m => GerritCrawler.GerritEnv -> DocumentStream m
    gerritChangesCrawler gerritEnv = Changes $ GerritCrawler.getChangesStream gerritEnv
