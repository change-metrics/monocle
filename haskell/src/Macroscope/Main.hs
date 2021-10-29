-- |
module Macroscope.Main (runMacroscope, getStream, getCrawlers, Clients (..)) where

import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import qualified Control.Scheduler as Scheduler (Comp (..), traverseConcurrently_)
import qualified Data.Map as Map
import qualified Data.Text as T
import Gerrit (GerritClient)
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
  deriving (Eq, Show)

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
  loop config (Clients mempty mempty mempty)
  where
    loop config clients = do
      -- Reload config
      conf <- snd <$> config

      -- Flatten each crawler from all workspaces
      let crawlerInfos = getCrawlers conf

      -- Create the streams and update the client store
      (streams, newClients) <- runStateT (traverse getStream crawlerInfos) clients

      -- Crawl each index
      let traverseParallel = Scheduler.traverseConcurrently_ Scheduler.Seq
      traverseParallel (traverse_ runCrawler) $ groupByClient $ streams

      -- Pause
      mLog $ Log Macroscope $ LogMacroPause interval
      mThreadDelay $ fromIntegral $ interval * 1_000_000

      -- Loop again
      loop config newClients

-- | 'Clients' is a store for all the remote clients, indexed using their url/token
data Clients = Clients
  { clientsGerrit :: Map (Text, Maybe (Text, Secret)) GerritClient,
    clientsBugzilla :: Map (Text, Secret) BugzillaSession,
    clientsGraph :: Map (Text, Secret) GraphClient
  }

instance From () Clients where
  from _ = Clients mempty mempty mempty

type ClientKey = Int

type GetClient m a = StateT Clients m (ClientKey, a)

-- | Boilerplate function to retrieve a client from the store
getClientGerrit :: MonadGerrit m => Text -> Maybe (Text, Secret) -> GetClient m GerritClient
getClientGerrit url auth = do
  clients <- gets clientsGerrit
  (client, newClients) <- mapMutate clients (url, auth) $ lift $ getGerritClient url auth
  modify $ \s -> s {clientsGerrit = newClients}
  pure (hashWithSalt 0 (url, auth), client)

-- | Boilerplate function to retrieve a client from the store
getClientBZ :: MonadBZ m => Text -> Secret -> GetClient m BugzillaSession
getClientBZ url token = do
  clients <- gets clientsBugzilla
  (client, newClients) <- mapMutate clients (url, token) $ lift $ getBugzillaSession url $ Just $ getApikey (unSecret token)
  modify $ \s -> s {clientsBugzilla = newClients}
  pure (hashWithSalt 0 (url, token), client)

-- | Boilerplate function to retrieve a client from the store
getClientGraphQL :: MonadGraphQL m => Text -> Secret -> GetClient m GraphClient
getClientGraphQL url token = do
  clients <- gets clientsGraph
  (client, newClients) <- mapMutate clients (url, token) $ lift $ newGraphClient url token
  modify $ \s -> s {clientsGraph = newClients}
  pure (hashWithSalt 0 (url, token), client)

groupByClient :: forall a. [(ClientKey, a)] -> [[a]]
groupByClient xs = fmap (map snd) grpL
  where
    -- group by the client key
    grp :: [NonEmpty (ClientKey, a)]
    grp = Map.elems $ groupBy fst xs
    -- transform to a regular list
    grpL :: [[(ClientKey, a)]]
    grpL = fmap toList grp

type MonadMacro m = (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m, MonadReader CrawlerEnv m)

runCrawler :: MonadMacro m => (CrawlerInfo, [DocumentStream m]) -> m ()
runCrawler = safeCrawl
  where
    safeCrawl :: MonadMacro m => (CrawlerInfo, [DocumentStream m]) -> m ()
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right () -> pure ()
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

-- 'getStream' converts the crawler configuration into a stream
getStream ::
  (Config.MonadConfig m, MonadGerrit m, MonadGraphQL m, MonadThrow m, MonadBZ m) =>
  CrawlerInfo ->
  StateT Clients m (ClientKey, (CrawlerInfo, [DocumentStream m]))
getStream ci@(CrawlerInfo _ _ crawler idents) = do
  (key, streams) <- getStream'
  pure $ (key, (ci, streams))
  where
    getStream' =
      -- Create document streams
      case Config.provider crawler of
        Config.GitlabProvider Config.Gitlab {..} -> do
          token <- lift $ Config.mGetSecret "GITLAB_TOKEN" gitlab_token
          (k, glClient) <-
            getClientGraphQL
              (fromMaybe "https://gitlab.com/api/graphql" gitlab_url)
              token
          let streams =
                [glOrgCrawler glClient | isNothing gitlab_repositories]
                  -- Then we always index the projects
                  <> [glMRCrawler glClient getIdentByAliasCB]
          pure $ (k, streams)
        Config.GerritProvider Config.Gerrit {..} -> do
          auth <- lift $ case gerrit_login of
            Just login -> do
              passwd <- Config.mGetSecret "GERRIT_PASSWORD" gerrit_password
              pure $ Just (login, passwd)
            Nothing -> pure Nothing
          (k, gClient) <- getClientGerrit gerrit_url auth
          let gerritEnv = GerritCrawler.GerritEnv gClient gerrit_prefix getIdentByAliasCB
              streams =
                [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
                  <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
          pure $ (k, streams)
        Config.BugzillaProvider Config.Bugzilla {..} -> do
          bzToken <- lift $ Config.mGetSecret "BUGZILLA_TOKEN" bugzilla_token
          (k, bzClient) <- getClientBZ bugzilla_url bzToken
          pure $ (k, [bzCrawler bzClient])
        Config.GithubProvider ghCrawler -> do
          let Config.Github _ _ github_token github_url = ghCrawler
          ghToken <- lift $ Config.mGetSecret "GITHUB_TOKEN" github_token
          (k, ghClient) <- getClientGraphQL (fromMaybe "https://api.github.com/graphql" github_url) ghToken
          pure (k, [ghIssuesCrawler ghClient])
        Config.GithubApplicationProvider _ -> error "Not (yet) implemented"
        Config.TaskDataProvider -> pure (0, []) -- This is a generic crawler, not managed by the macroscope
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
