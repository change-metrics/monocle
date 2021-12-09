-- | This module implements the main macroscope process to manage the lentilles.
-- The goal is to run the lentilles in parallel using one thread per endpoint,
-- so that a single rate limit doesn't block every lentille.
--
-- Note that one crawler can have multiple lentilles, for example a gerrit crawler has a
-- project lentille to collect the repository list, and a change lentille to collect the pull requests.
module Macroscope.Main (runMacroscope, getCrawler, getCrawlers, Clients (..), runCrawlers, runCrawlers', mkStreamsActions) where

import qualified Data.List.NonEmpty as NonEmpty
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
import Prometheus.Metric.GHC (ghcMetrics)
import qualified UnliftIO.Async as Async

-- | A structure to carry a single crawler information.
data InfoCrawler = InfoCrawler
  { infoWorkspaceName :: Text,
    infoCrawlerKey :: Text,
    infoCrawler :: Config.Crawler,
    infoIdents :: [Config.Ident]
  }
  deriving (Eq, Show)

-- | A crawler is defined from the config, using a tuple: (crawler, list of lentille)
type Crawler m = (InfoCrawler, [DocumentStream m])

-- | Utility function to create a flat list of crawler from the whole configuration
getCrawlers :: [Config.Index] -> [InfoCrawler]
getCrawlers xs = do
  Config.Index {..} <- xs
  infoCrawler <- crawlers
  let infoCrawlerKey = fromMaybe (error "unknown crawler key") crawlers_api_key
      infoIdents = fromMaybe [] idents
      infoWorkspaceName = name
  pure $ InfoCrawler {..}

crawlerName :: Config.Crawler -> Text
crawlerName Config.Crawler {..} = name

withMonitoringServer :: Int -> IO () -> IO ()
withMonitoringServer port action = do
  -- Setup GHC metrics for prometheus
  void $ promRegister ghcMetrics

  -- Start the Wai application in the background with Warp
  v <- newEmptyMVar
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar v ()) Warp.defaultSettings
  withAsync (Warp.runSettings settings app) $ \warpPid -> do
    mLog $ Log Macroscope $ LogStartingMonitoring port
    -- Wait for the warp service to be running
    takeMVar v
    -- Run the action
    action
    cancel warpPid
  where
    app req resp = case Wai.rawPathInfo req of
      "/health" -> resp $ Wai.responseLBS HTTP.status200 [] "macroscope is running\n"
      "/metrics" -> resp . Wai.responseLBS HTTP.ok200 [] =<< exportMetricsAsText
      _anyOtherPath -> resp $ Wai.responseLBS HTTP.notFound404 [] mempty

-- | 'runMacroscope is the entrypoint of the macroscope process
-- withClient "http://localhost:8080" Nothing $ \client -> runMacroscope 9001 "/home/user/git/github.com/change-metrics/monocle/etc/config.yaml" client
runMacroscope :: Int -> FilePath -> MonocleClient -> IO ()
runMacroscope port confPath client = withMonitoringServer port $ do
  runLentilleM client $ do
    mLog $ Log Macroscope LogMacroStart
    config <- Config.mReloadConfig confPath
    loop config (from ())
  where
    loop config clients = do
      -- Load the config
      mLog $ Log Macroscope LogMacroStart
      conf <- Config.csConfig <$> config

      -- Flatten each crawler from all workspaces
      let crawlerInfos = getCrawlers $ Config.getWorkspaces conf

      -- Create the streams and update the client store
      (streams, newClients) <- runStateT (traverse getCrawler crawlerInfos) clients

      -- Run the steams group
      runCrawlers (Config.csReloaded <$> config) (mkStreamsActions $ catMaybes streams)

      -- Loop again
      loop config newClients

-- | A stream group is defined by a tuple: (group name, list of action)
type StreamGroup m = (Text, NonEmpty (m ()))

-- | Creates the action `m ()` for each `DocumentStream m` using 'runCrawler'
mkStreamsActions :: MonadMacro m => [(ClientKey, Crawler m)] -> [StreamGroup m]
mkStreamsActions = map mkStreamGroup . groupByClient
  where
    mkStreamGroup :: MonadMacro m => (ClientKey, NonEmpty (Crawler m)) -> StreamGroup m
    mkStreamGroup (k, xs) = (k <> " for " <> crawlersName xs, fmap runCrawler xs)

    crawlersName :: NonEmpty (Crawler m) -> Text
    crawlersName = T.intercalate ", " . map (crawlerName . infoCrawler . fst) . toList

-- | Continuously runs the stream groups in parallel until the config is reloaded
runCrawlers :: (MonadUnliftIO m, MonadMacro m) => m Bool -> [StreamGroup m] -> m ()
runCrawlers = runCrawlers' 500_000 600_000_000 30_000_000

-- | The runCrawler implementation with custom delay for testing purpose
runCrawlers' ::
  (MonadUnliftIO m, MonadMacro m) =>
  -- | How long to wait before starting further crawler
  Int ->
  -- | How long to wait before restarting a crawler
  Int ->
  -- | How long to wait before checking if the config changed
  Int ->
  -- | The action to check if the config changed
  m Bool ->
  -- | The list of stream group
  [StreamGroup m] ->
  m ()
runCrawlers' startDelay loopDelay watchDelay isReloaded groups = do
  mLog $ Log Macroscope $ LogMacroStartCrawlers $ map fst groups
  -- Create a 'runGroup' thread for each stream group
  let groupAsyncs = Async.mapConcurrently runGroup (zip [0 ..] groups)

  -- Then watch for config change
  Async.withAsync groupAsyncs watch
  where
    runGroup (delay, grp) = do
      -- Delay group start to avoid initial burst
      mThreadDelay $ startDelay * delay
      runGroup' grp

    runGroup' (groupName, streams) = do
      -- Evaluate the group streams in sequence
      mLog $ Log Macroscope $ LogMacroGroupStart groupName
      -- TODO: catch any exception and continue
      sequence_ streams
      mLog $ Log Macroscope $ LogMacroGroupEnd groupName

      -- Pause the group
      unlessStopped $ pauseGroup 0
      -- Keep on running the group until the configuration changed
      unlessStopped $ runGroup' (groupName, streams)

    pauseGroup x
      | x > loopDelay = pure () -- The pause completed
      | otherwise = do
        let step = min loopDelay 1_000_000
        -- Pause for one second
        mThreadDelay step
        -- Then continue
        unlessStopped $ pauseGroup (x + step)

    watch groupAsyncs = do
      -- Check if the config changed
      reloaded <- isReloaded
      if reloaded
        then do
          -- Update the crawlerStop ref to True so that stream gracefully stops
          mLog $ Log Macroscope LogMacroReloadingStart
          ref <- asks crawlerStop
          liftIO $ writeIORef ref True

          -- Wait for completion (TODO: use Async.poll for 1 hour, then force thread terminate)
          _res <- Async.wait groupAsyncs
          -- TODO: log exceptions in _res

          -- Reset the stop ref
          liftIO $ writeIORef ref False
          pure ()
        else do
          -- otherwise pause before starting again
          mThreadDelay watchDelay
          watch groupAsyncs

-- | 'Clients' is a store for all the remote clients, indexed using their url/token
data Clients = Clients
  { clientsGerrit :: Map (Text, Maybe (Text, Secret)) GerritClient,
    clientsBugzilla :: Map (Text, Secret) BugzillaSession,
    clientsGraph :: Map (Text, Secret) GraphClient
  }

-- | Create a default Clients store from the unit
instance From () Clients where
  from _ = Clients mempty mempty mempty

type ClientKey = Text

-- | GetClient m a is a convenient alias that means:
--   this is a computation that:
--     * needs a 'Clients' state,
--     * produces a tuple (ClientKey, @a@)
--     * using an inner context @m@
type GetClient m a = StateT Clients m (ClientKey, a)

-- | Boilerplate function to retrieve a client from the store
getClientGerrit :: MonadGerrit m => Text -> Maybe (Text, Secret) -> GetClient m GerritClient
getClientGerrit url auth = do
  clients <- gets clientsGerrit
  (client, newClients) <- mapMutate clients (url, auth) $ lift $ getGerritClient url auth
  modify $ \s -> s {clientsGerrit = newClients}
  pure (url, client)

-- | Boilerplate function to retrieve a client from the store
getClientBZ :: MonadBZ m => Text -> Secret -> GetClient m BugzillaSession
getClientBZ url token = do
  clients <- gets clientsBugzilla
  (client, newClients) <- mapMutate clients (url, token) $ lift $ getBugzillaSession url $ Just $ getApikey (unSecret token)
  modify $ \s -> s {clientsBugzilla = newClients}
  pure (url, client)

-- | Boilerplate function to retrieve a client from the store
getClientGraphQL :: MonadGraphQL m => "crawler" ::: Text -> Text -> Secret -> GetClient m GraphClient
getClientGraphQL crawler url token = do
  clients <- gets clientsGraph
  (client, newClients) <- mapMutate clients (url, token) $ lift $ newGraphClient crawler url token
  modify $ \s -> s {clientsGraph = newClients}
  let groupKey = url <> "-" <> unSecret token
  pure (groupKey, client)

-- | Groups the streams by client
--
-- >>> map (fmap toList) $ groupByClient [(0, "gitlab"), (1, "gerrit"), (0, "gitlab 2")] :: [(Int, [String])]
-- [(0,["gitlab","gitlab 2"]),(1,["gerrit"])]
groupByClient :: Ord clientKey => [(clientKey, a)] -> [(clientKey, NonEmpty a)]
groupByClient = grp >>> adapt
  where
    -- group by the client key using
    -- https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Extra-Group.html#v:groupBy
    grp :: Ord clientKey => [(clientKey, a)] -> [(clientKey, NonEmpty (clientKey, a))]
    grp = Map.toList . groupBy fst

    adapt :: [(clientKey, NonEmpty (clientKey, a))] -> [(clientKey, NonEmpty a)]
    adapt = map (fmap keepOrder)

    -- groupBy produced a list that needs to be reversed to keep the order
    keepOrder :: NonEmpty (clientKey, a) -> NonEmpty a
    keepOrder = fmap snd . NonEmpty.reverse

-- | MonadMacro is an alias for a bunch of constraints
type MonadMacro m = (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m, MonadReader CrawlerEnv m)

-- | 'runCrawler' evaluate a single crawler
runCrawler :: MonadMacro m => Crawler m -> m ()
runCrawler = safeCrawl
  where
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right () -> pure ()
        Left exc ->
          let (InfoCrawler index _ Config.Crawler {..} _, _) = crawler
           in mLog $ Log Macroscope $ LogMacroSkipCrawler (LogCrawlerContext index name) (show exc)

    crawl :: MonadMacro m => Crawler m -> m ()
    crawl (InfoCrawler index key crawler _, docStreams) = do
      mLog $ Log Macroscope $ LogMacroStartCrawler $ LogCrawlerContext index (crawlerName crawler)

      let runner = runStream (toLazy key) (toLazy index) (toLazy $ crawlerName crawler)
      traverse_ runner docStreams

-- | 'getCrawler' converts a crawler configuration into a (ClientKey, streams)
getCrawler ::
  (Config.MonadConfig m, MonadGerrit m, MonadGraphQL m, MonadThrow m, MonadBZ m) =>
  InfoCrawler ->
  StateT Clients m (Maybe (ClientKey, Crawler m))
getCrawler inf@(InfoCrawler _ _ crawler idents) = getCompose $ fmap addInfos (Compose getStreams)
  where
    addInfos (key, streams) = (key, (inf, streams))
    getStreams =
      case Config.provider crawler of
        Config.GitlabProvider Config.Gitlab {..} -> do
          token <- lift $ Config.mGetSecret "GITLAB_TOKEN" gitlab_token
          (k, glClient) <-
            getClientGraphQL
              (Config.getCrawlerName crawler)
              (fromMaybe "https://gitlab.com/api/graphql" gitlab_url)
              token
          let streams =
                [glOrgCrawler glClient | isNothing gitlab_repositories]
                  -- Then we always index the projects
                  <> [glMRCrawler glClient getIdentByAliasCB]
          pure $ Just (k, streams)
        Config.GerritProvider Config.Gerrit {..} -> do
          auth <- lift $ case gerrit_login of
            Just login -> do
              passwd <- Config.mGetSecret "GERRIT_PASSWORD" gerrit_password
              pure $ Just (login, passwd)
            Nothing -> pure Nothing
          (k, gClient) <- getClientGerrit gerrit_url auth
          let gerritEnv = GerritCrawler.GerritEnv gClient gerrit_prefix getIdentByAliasCB (Config.getCrawlerName crawler)
              streams =
                [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
                  <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
          pure $ Just (k, streams)
        Config.BugzillaProvider Config.Bugzilla {..} -> do
          bzToken <- lift $ Config.mGetSecret "BUGZILLA_TOKEN" bugzilla_token
          (k, bzClient) <- getClientBZ bugzilla_url bzToken
          pure $ Just (k, [bzCrawler bzClient])
        Config.GithubProvider ghCrawler -> do
          let Config.Github _ _ github_token github_url = ghCrawler
          ghToken <- lift $ Config.mGetSecret "GITHUB_TOKEN" github_token
          (k, ghClient) <- getClientGraphQL (Config.getCrawlerName crawler) (fromMaybe "https://api.github.com/graphql" github_url) ghToken
          pure $ Just (k, [ghIssuesCrawler ghClient])
        Config.GithubApplicationProvider _ -> pure Nothing -- "Not (yet) implemented"
        Config.TaskDataProvider -> pure Nothing -- This is a generic crawler, not managed by the macroscope
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
