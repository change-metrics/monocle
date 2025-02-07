-- | This module implements the main macroscope process to manage the lentilles.
-- The goal is to run the lentilles in parallel using one thread per endpoint,
-- so that a single rate limit doesn't block every lentille.
--
-- Note that one crawler can have multiple lentilles, for example a gerrit crawler has a
-- project lentille to collect the repository list, and a change lentille to collect the pull requests.
module Macroscope.Main (
  -- * entry point
  runMacroscope,
  runMacroEffects,

  -- * helpers exported for testing
  getCrawler,
  getCrawlers,
  Clients (..),
  runCrawlers,
  runCrawlers',
  mkStreamsActions,
) where

import Data.Hashable (hash)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as T
import Gerrit (GerritClient)
import Lentille
import Lentille.Bugzilla
import Lentille.Gerrit hiding (crawlerName)
import Lentille.Gerrit qualified as GerritCrawler (GerritEnv (..), getChangesStream, getProjectsStream)
import Lentille.GitHub.Issues (streamLinkedIssue)
import Lentille.GitHub.Organization (streamOrganizationProjects)
import Lentille.GitHub.PullRequests (streamPullRequests)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Lentille.GraphQL
import Macroscope.Worker (DocumentStream (..), runStream)
import Monocle.Client
import Monocle.Config qualified as Config
import Monocle.Entity (CrawlerName (..))
import Monocle.Prelude
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Prometheus.Metric.GHC (ghcMetrics)

import Effectful.Concurrent.Async qualified as E
import Effectful.Concurrent.MVar qualified as E
import Effectful.Env
import Effectful.Prometheus
import Effectful.Reader.Static qualified as E
import Effectful.Timeout (Timeout, runTimeout)
import Lentille.GitHub.UserPullRequests (streamUserPullRequests)
import Monocle.Effects

-- | A structure to carry a single crawler information.
data InfoCrawler = InfoCrawler
  { infoWorkspaceName :: Config.IndexName
  , infoCrawlerKey :: Text
  , infoCrawler :: Config.Crawler
  , infoIdents :: [Config.Ident]
  }
  deriving (Eq, Show)

-- | A crawler is defined from the config, using a tuple: (crawler, list of lentille)
type Crawler es = (InfoCrawler, [DocumentStream es])

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

withMonitoringServer :: (IOE :> es, LoggerEffect :> es, E.Concurrent :> es) => Int -> Eff es () -> Eff es ()
withMonitoringServer port action = do
  -- Setup GHC metrics for prometheus
  void $ promRegister ghcMetrics

  -- Start the Wai application in the background with Warp
  v <- E.newEmptyMVar
  let settings = Warp.setPort port $ Warp.setBeforeMainLoop (putMVar v ()) Warp.defaultSettings
  E.withAsync (liftIO $ Warp.runSettings settings app) $ \warpPid -> do
    logInfo "Starting monitoring service" ["port" .= port]
    -- Wait for the warp service to be running
    takeMVar v
    -- Run the action
    action
    E.cancel warpPid
 where
  app req resp = case Wai.rawPathInfo req of
    "/health" -> resp $ Wai.responseLBS HTTP.status200 [] "macroscope is running\n"
    "/metrics" -> resp . Wai.responseLBS HTTP.ok200 [] =<< exportMetricsAsText
    _anyOtherPath -> resp $ Wai.responseLBS HTTP.notFound404 [] mempty

-- | 'runMacroscope is the entrypoint of the macroscope process
-- withClient "http://localhost:8080" Nothing $ runMacroscope 9001 "../etc/config.yaml"
runMacroscope :: Int -> FilePath -> MonocleClient -> IO ()
runMacroscope port confPath client = do
  crawlerEnv <- CrawlerEnv client <$> newIORef False
  runEff
    . runMacroEffects
    . runLoggerEffect
    . runMonoConfig confPath
    . E.runReader crawlerEnv
    . runMonoClient client
    $ withMonitoringServer port runMacroscope'

runMacroscope' :: forall es. (IOE :> es, MonoConfigEffect :> es, MacroEffects es) => Eff es ()
runMacroscope' = do
  logInfo_ "Starting to fetch streams"
  loop (Clients mempty mempty mempty)
 where
  loop :: Clients -> Eff es ()
  loop clients = do
    -- Load the config
    conf <- Config.csConfig <$> getReloadConfig
    let loopDelay = 1_000_000 * fromIntegral (fromMaybe 600 ((.loop_delay_sec) =<< conf.crawlers))

    -- Flatten each crawler from all workspaces
    let crawlerInfos = getCrawlers $ Config.getWorkspaces conf

    -- Create the streams and update the client store
    (streams, newClients) <- runStateT (traverse getCrawler crawlerInfos) clients

    -- Run the steams group
    runCrawlers loopDelay (Config.csReloaded <$> getReloadConfig) (mkStreamsActions $ catMaybes streams)

    -- Loop again
    loop newClients

-- | A stream group is defined by a tuple: (group name, list of action)
type StreamGroup es = (Text, NonEmpty (Eff es ()))

-- | Creates the action `m ()` for each `DocumentStream m` using 'runCrawler'
mkStreamsActions :: forall es. MacroEffects es => [(ClientKey, Crawler es)] -> [StreamGroup es]
mkStreamsActions = map mkStreamGroup . groupByClient
 where
  mkStreamGroup :: (ClientKey, NonEmpty (Crawler es)) -> StreamGroup es
  mkStreamGroup (k, xs) = (k <> " for " <> crawlersName xs, fmap runCrawler xs)

  crawlersName :: NonEmpty (Crawler es) -> Text
  crawlersName = T.intercalate ", " . map (crawlerName . infoCrawler . fst) . toList

-- | Continuously runs the stream groups in parallel until the config is reloaded
runCrawlers :: (IOE :> es, MacroEffects es) => Int -> Eff es Bool -> [StreamGroup es] -> Eff es ()
runCrawlers = runCrawlers' 500_000 30_000_000

-- | The runCrawler implementation with custom delay for testing purpose
runCrawlers' ::
  (IOE :> es, MacroEffects es) =>
  -- | How long to wait before starting further crawler
  Int ->
  -- | How long to wait before checking if the config changed
  Int ->
  -- | How long to wait before restarting a crawler
  Int ->
  -- | The action to check if the config changed
  Eff es Bool ->
  -- | The list of stream group
  [StreamGroup es] ->
  Eff es ()
runCrawlers' startDelay watchDelay loopDelay isReloaded groups = do
  logInfo "Starting crawlers" ["crawlers" .= map fst groups]
  -- Create a 'runGroup' thread for each stream group
  let groupAsyncs = E.mapConcurrently runGroup (zip [0 ..] groups)

  -- Then watch for config change
  E.withAsync groupAsyncs watch
 where
  runGroup (delay, grp) = do
    -- Delay group start to avoid initial burst
    mThreadDelay $ startDelay * delay
    runGroup' grp

  runGroup' (groupName, streams) = do
    -- Evaluate the group streams in sequence
    logInfo "Group starting" ["group" .= groupName]
    -- TODO: catch any exception and continue
    sequence_ streams
    logInfo "Group end" ["group" .= groupName]

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
        logInfo_ "Macroscope reloading begin"
        ref <- E.asks crawlerStop
        liftIO $ writeIORef ref True

        -- Wait for completion (TODO: use Async.poll for 1 hour, then force thread terminate)
        _res <- E.wait groupAsyncs
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
  { clientsGerrit :: Map (Text, Maybe (Text, Secret)) GerritClient
  , clientsBugzilla :: Map (Text, Secret) BugzillaSession
  , clientsGraph :: Map (Text, Secret) GraphClient
  }

type ClientKey = Text

-- | GetClient m a is a convenient alias that means:
--   this is a computation that:
--     * needs a 'Clients' state,
--     * produces a tuple (ClientKey, @a@)
--     * using an inner context @m@
type GetClient es a = StateT Clients (Eff es) (ClientKey, a)

-- | Boilerplate function to retrieve a client from the store
getClientGerrit :: GerritEffect :> es => Text -> Maybe (Text, Secret) -> GetClient es GerritClient
getClientGerrit url auth = do
  clients <- gets clientsGerrit
  (client, newClients) <- mapMutate clients (url, auth) $ lift $ getGerritClient url auth
  modify $ \s -> s {clientsGerrit = newClients}
  pure (url, client)

-- | Boilerplate function to retrieve a client from the store
getClientBZ :: Text -> Secret -> GetClient es BugzillaSession
getClientBZ url token = do
  clients <- gets clientsBugzilla
  (client, newClients) <-
    mapMutate clients (url, token)
      . pure
      $ getBugzillaSession url
      $ Just
      $ getApikey token
  modify $ \s -> s {clientsBugzilla = newClients}
  pure (url, client)

-- | Boilerplate function to retrieve a client from the store
getClientGraphQL :: Concurrent :> es => Text -> Secret -> GetClient es GraphClient
getClientGraphQL url token = do
  clients <- gets clientsGraph
  (client, newClients) <- mapMutate clients (url, token) $ lift $ newGraphClient url token
  modify $ \s -> s {clientsGraph = newClients}
  let groupKey = url <> "-" <> show (hash token)
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

-- | MonadMacro is an alias for a bunch of constraints required for the macroscope process
type MacroEffects es =
  ( GerritEffect :> es
  , BZEffect :> es
  , E.Reader CrawlerEnv :> es
  , MonoClientEffect :> es
  , HttpEffect :> es
  , PrometheusEffect :> es
  , LoggerEffect :> es
  , TimeEffect :> es
  , EnvEffect :> es
  , Retry :> es
  , Concurrent :> es
  , Fail :> es
  , Timeout :> es
  )

runMacroEffects :: IOE :> es => Eff (GerritEffect : BZEffect : TimeEffect : HttpEffect : PrometheusEffect : EnvEffect : Fail : Retry : Concurrent : Timeout : es) a -> Eff es a
runMacroEffects = runTimeout . runConcurrent . runRetry . runFailIO . runEnv . runPrometheus . runHttpEffect . runTime . runBZ . runGerrit

-- | 'runCrawler' evaluate a single crawler
runCrawler :: forall es. MacroEffects es => Crawler es -> Eff es ()
runCrawler = safeCrawl
 where
  safeCrawl crawler = do
    catched <- tryAny $ crawl crawler
    case catched of
      Right () -> pure ()
      Left exc ->
        let (InfoCrawler index _ Config.Crawler {..} _, _) = crawler
         in logWarn "Skipping due to an unexpected exception" ["index" .= index, "crawler" .= name, "err" .= show @Text exc]

  crawl :: Crawler es -> Eff es ()
  crawl (InfoCrawler index key crawler _, docStreams) = do
    logInfo "Starting crawler" ["index" .= index, "crawler" .= crawlerName crawler]

    let runner = runStream (from key) (from index) (CrawlerName $ crawlerName crawler)
    traverse_ runner docStreams

-- | 'getCrawler' converts a crawler configuration into a (ClientKey, streams)
getCrawler ::
  forall es.
  MacroEffects es =>
  InfoCrawler ->
  StateT Clients (Eff es) (Maybe (ClientKey, Crawler es))
getCrawler inf@(InfoCrawler _ _ crawler idents) = getCompose $ fmap addInfos (Compose getStreams)
 where
  addInfos (key, streams) = (key, (inf, streams))
  getStreams =
    case Config.provider crawler of
      Config.GitlabProvider Config.Gitlab {..} -> do
        token <- lift $ Config.getSecret "GITLAB_TOKEN" gitlab_token
        (k, glClient) <-
          getClientGraphQL
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
            passwd <- Config.getSecret "GERRIT_PASSWORD" gerrit_password
            pure $ Just (login, passwd)
          Nothing -> pure Nothing
        (k, gClient) <- getClientGerrit gerrit_url auth
        let gerritEnv = GerritCrawler.GerritEnv gClient gerrit_prefix getIdentByAliasCB (Config.getCrawlerName crawler)
            streams =
              [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
                <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
        pure $ Just (k, streams)
      Config.BugzillaProvider Config.Bugzilla {..} -> do
        bzToken <- lift $ Config.getSecret "BUGZILLA_TOKEN" bugzilla_token
        (k, bzClient) <- getClientBZ bugzilla_url bzToken
        pure $ Just (k, [bzCrawler bzClient])
      Config.GithubProvider ghCrawler -> do
        let Config.Github {..} = ghCrawler
        (k, ghClient) <- getGHClient github_token github_url
        let crawlers =
              [ghOrgCrawler ghClient | isNothing github_repositories]
                <> [ghIssuesCrawler ghClient]
                <> [ghPRCrawler ghClient getIdentByAliasCB]
        pure $ Just (k, crawlers)
      Config.GithubUserProvider ghUserCrawler -> do
        let Config.GithubUser {..} = ghUserCrawler
        (k, ghClient) <- getGHClient github_token github_url
        pure $ Just (k, [ghUserPRCrawler ghClient getIdentByAliasCB])
      Config.GithubApplicationProvider _ -> pure Nothing -- "Not (yet) implemented"
      Config.TaskDataProvider -> pure Nothing -- This is a generic crawler, not managed by the macroscope
  getIdentByAliasCB :: Text -> Maybe (Text, [Text])
  getIdentByAliasCB = flip Config.getIdentByAliasFromIdents idents

  getGHClient mToken mAPIUrl = do
    ghToken <- lift $ Config.getSecret "GITHUB_TOKEN" mToken
    getClientGraphQL
      (fromMaybe "https://api.github.com/graphql" mAPIUrl)
      ghToken

  glMRCrawler :: GraphClient -> (Text -> Maybe Config.IdentUG) -> DocumentStream es
  glMRCrawler glClient cb = Changes $ streamMergeRequests glClient cb

  glOrgCrawler :: GraphClient -> DocumentStream es
  glOrgCrawler glClient = Projects $ streamGroupProjects glClient

  bzCrawler :: BugzillaSession -> DocumentStream es
  bzCrawler bzSession = TaskDatas $ getBZData bzSession

  ghIssuesCrawler :: GraphClient -> DocumentStream es
  ghIssuesCrawler ghClient = TaskDatas $ streamLinkedIssue ghClient

  ghOrgCrawler :: GraphClient -> DocumentStream es
  ghOrgCrawler ghClient = Projects $ streamOrganizationProjects ghClient

  ghPRCrawler :: GraphClient -> (Text -> Maybe Config.IdentUG) -> DocumentStream es
  ghPRCrawler glClient cb = Changes $ streamPullRequests glClient cb

  ghUserPRCrawler :: GraphClient -> (Text -> Maybe Config.IdentUG) -> DocumentStream es
  ghUserPRCrawler glClient cb = UserChanges $ streamUserPullRequests glClient cb

  gerritRegexProjects :: [Text] -> [Text]
  gerritRegexProjects = filter (T.isPrefixOf "^")

  gerritREProjectsCrawler :: GerritCrawler.GerritEnv -> DocumentStream es
  gerritREProjectsCrawler gerritEnv = Projects $ GerritCrawler.getProjectsStream gerritEnv

  gerritChangesCrawler :: GerritCrawler.GerritEnv -> DocumentStream es
  gerritChangesCrawler gerritEnv = Changes $ GerritCrawler.getChangesStream gerritEnv
