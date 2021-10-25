-- |
module Macroscope.Main (runMacroscope) where

import Control.Exception.Safe (tryAny)
import qualified Data.Text as T
import Lentille
import Lentille.Bugzilla (BugzillaSession, MonadBZ, getApikey, getBZData, getBugzillaSession)
import Lentille.Gerrit (MonadGerrit (..))
import qualified Lentille.Gerrit as GerritCrawler (GerritEnv, getChangesStream, getGerritEnv, getProjectsStream)
import Lentille.GitHub.Issues (streamLinkedIssue)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Lentille.GraphQL
import Macroscope.Worker (DocumentStream (..), runStream)
import qualified Monocle.Api.Config as Config
import Monocle.Client
import Monocle.Prelude

-- | Utility function to create a flat list of crawler from the whole configuration
getCrawlers :: [Config.Index] -> [(Text, Text, Config.Crawler, [Config.Ident])]
getCrawlers xs = do
  Config.Index {..} <- xs
  crawler <- crawlers
  let key = fromMaybe (error "unknown crawler key") crawlers_api_key
  pure (name, key, crawler, fromMaybe [] idents)

crawlerName :: Config.Crawler -> Text
crawlerName Config.Crawler {..} = name

-- | 'run' is the entrypoint of the macroscope process
-- withClient "http://localhost:8080" Nothing $ \client -> runMacroscope True "/home/user/git/github.com/change-metrics/monocle/etc/config.yaml" 30 client
runMacroscope :: Bool -> FilePath -> Word32 -> MonocleClient -> IO ()
runMacroscope verbose confPath interval client = do
  res <- runLentilleM $ runMacroscope' verbose confPath interval client
  case res of
    Left e -> error $ "Macroscope failed: " <> show e
    Right x -> pure x

runMacroscope' :: (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m) => Bool -> FilePath -> Word32 -> MonocleClient -> m ()
runMacroscope' verbose confPath interval client = do
  mLog $ Log Macroscope LogMacroStart
  config <- Config.mReloadConfig confPath
  loop config
  where
    loop config = do
      -- Reload config
      conf <- config

      -- Crawl each index
      traverse_ safeCrawl (getCrawlers conf)

      -- Pause
      mLog $ Log Macroscope $ LogMacroPause interval_sec
      mThreadDelay interval_usec

      -- Loop again
      loop config

    interval_usec = fromInteger . toInteger $ interval * 1_000_000
    interval_sec :: Float
    interval_sec = fromIntegral interval_usec / 1_000_000

    safeCrawl :: (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m) => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right comp -> pure comp
        Left exc ->
          let (index, _, Config.Crawler {..}, _) = crawler
           in mLog $ Log Macroscope $ LogMacroSkipCrawler (LogCrawlerContext index name) (show exc)

    crawl :: (MonadCatch m, MonadGerrit m, MonadBZ m, LentilleMonad m) => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    crawl (index, key, crawler, idents) = do
      now <- toMonocleTime <$> mGetCurrentTime
      when verbose (mLog $ Log Macroscope $ LogMacroStartCrawler $ LogCrawlerContext index (crawlerName crawler))

      -- Create document streams
      docStreams <- case Config.provider crawler of
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
          let gerritEnv = GerritCrawler.getGerritEnv gClient gerrit_prefix $ Just getIdentByAliasCB
          pure $
            [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
              <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
        Config.BugzillaProvider Config.Bugzilla {..} -> do
          bzTokenT <- Config.mGetSecret "BUGZILLA_TOKEN" bugzilla_token
          bzClient <- getBugzillaSession bugzilla_url $ Just $ getApikey bzTokenT
          pure [bzCrawler bzClient]
        Config.GithubProvider ghCrawler -> do
          let Config.Github _ _ github_token github_url = ghCrawler
          ghToken <- Config.mGetSecret "GITHUB_TOKEN" github_token
          ghClient <- newGraphClient (fromMaybe "https://api.github.com/graphql" github_url) ghToken
          pure [ghIssuesCrawler ghClient]
        _ -> pure []

      -- Consume each stream
      let runner' = runStream client now (toLazy key) (toLazy index) (toLazy $ crawlerName crawler)

      let runner ds = case ds of
            Projects _ -> runner' ds
            Changes _ -> runner' ds
            TaskDatas _ -> runner' ds

      -- TODO: handle exceptions
      traverse_ runner docStreams
      where
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
