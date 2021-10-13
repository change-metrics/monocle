-- |
module Macroscope.Main (runMacroscope) where

import Control.Exception.Safe (tryAny)
import qualified Data.Text as T
import Lentille
import Lentille.Bugzilla (BugzillaSession, getApikey, getBZData, getBugzillaSession)
import qualified Lentille.Gerrit as GerritCrawler (GerritEnv, getChangesStream, getClient, getGerritEnv, getProjectsStream)
import Lentille.GitHub (GitHubGraphClient, githubDefaultGQLUrl, newGithubGraphClientWithKey)
import Lentille.GitHub.Issues (streamLinkedIssue)
import Lentille.GitLab (GitLabGraphClient, newGitLabGraphClientWithKey)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
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

-- | TODO, add the missing bits to the LentilleMonad to drop the MonadIO constraint, e.g.
--   reloadConfig, threadDelay, monocleClient
runMacroscope' :: MonadIO m => LentilleMonad m => Bool -> FilePath -> Word32 -> MonocleClient -> m ()
runMacroscope' verbose confPath interval client = do
  logRaw "Macroscope begin..."
  config <- liftIO $ Config.reloadConfig confPath
  loop config
  where
    loop config = do
      -- Reload config
      conf <- liftIO $ config

      -- Crawl each index
      traverse_ safeCrawl (getCrawlers conf)

      -- Pause
      logRaw $ "Waiting " <> show (fromIntegral interval_usec / 1_000_000 :: Float) <> "s. brb"
      liftIO $ threadDelay interval_usec

      -- Loop again
      loop config

    interval_usec = fromInteger . toInteger $ interval * 1_000_000

    safeCrawl :: MonadIO m => LentilleMonad m => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right comp -> pure comp
        Left exc ->
          let (_, _, Config.Crawler {..}, _) = crawler
           in logRaw $
                "Skipping crawler: " <> name <> ". Unexpected exception catched: " <> show exc

    crawl :: MonadIO m => LentilleMonad m => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    crawl (index, key, crawler, idents) = do
      now <- toMonocleTime <$> getCurrentTime
      when verbose (logRaw $ "Crawling " <> crawlerName crawler)

      -- Create document streams
      docStreams <- case Config.provider crawler of
        Config.GitlabProvider Config.Gitlab {..} -> do
          -- TODO: the client may be created once for each api key
          token <- Config.getSecret "GITLAB_TOKEN" gitlab_token
          glClient <-
            newGitLabGraphClientWithKey
              (fromMaybe "https://gitlab.com/api/graphql" gitlab_url)
              token
          pure $
            [glOrgCrawler glClient | isNothing gitlab_repositories]
              -- Then we always index the projects
              <> [glMRCrawler glClient getIdentByAliasCB]
        Config.GerritProvider Config.Gerrit {..} -> do
          auth <- case gerrit_login of
            Just login -> do
              passwd <- Config.getSecret "GERRIT_PASSWORD" gerrit_password
              pure $ Just (login, passwd)
            Nothing -> pure Nothing
          gClient <- liftIO $ GerritCrawler.getClient gerrit_url auth
          let gerritEnv = GerritCrawler.getGerritEnv gClient gerrit_prefix $ Just getIdentByAliasCB
          pure $
            [gerritREProjectsCrawler gerritEnv | maybe False (not . null . gerritRegexProjects) gerrit_repositories]
              <> [gerritChangesCrawler gerritEnv | isJust gerrit_repositories]
        Config.BugzillaProvider Config.Bugzilla {..} -> do
          bzTokenT <- Config.getSecret "BUGZILLA_TOKEN" bugzilla_token
          bzClient <- getBugzillaSession bugzilla_url $ Just $ getApikey bzTokenT
          pure $ bzCrawler bzClient <$> fromMaybe [] bugzilla_products
        Config.GithubProvider ghCrawler -> do
          let Config.Github _ _ github_token github_url = ghCrawler
          ghToken <- Config.getSecret "GITHUB_TOKEN" github_token
          ghClient <- newGithubGraphClientWithKey (fromMaybe githubDefaultGQLUrl github_url) ghToken
          let repos = Config.getCrawlerProject crawler
          pure $ ghIssuesCrawler ghClient <$> repos
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

    glMRCrawler :: MonadGraphQL m => GitLabGraphClient -> (Text -> Maybe Text) -> DocumentStream m
    glMRCrawler glClient cb = Changes $ streamMergeRequests glClient cb

    glOrgCrawler :: MonadGraphQL m => GitLabGraphClient -> DocumentStream m
    glOrgCrawler glClient = Projects $ streamGroupProjects glClient

    bzCrawler :: MonadBZ m => BugzillaSession -> Text -> DocumentStream m
    bzCrawler bzSession bzProduct = TaskDatas $ getBZData bzSession bzProduct

    ghIssuesCrawler :: MonadGraphQL m => GitHubGraphClient -> Text -> DocumentStream m
    ghIssuesCrawler ghClient repository =
      TaskDatas $
        streamLinkedIssue ghClient $ toString $ "repo:" <> repository

    gerritRegexProjects :: [Text] -> [Text]
    gerritRegexProjects projects = filter (T.isPrefixOf "^") projects

    gerritREProjectsCrawler :: MonadIO m => GerritCrawler.GerritEnv -> DocumentStream m
    gerritREProjectsCrawler gerritEnv = Projects $ GerritCrawler.getProjectsStream gerritEnv

    gerritChangesCrawler :: MonadIO m => GerritCrawler.GerritEnv -> DocumentStream m
    gerritChangesCrawler gerritEnv = Changes $ GerritCrawler.getChangesStream gerritEnv
