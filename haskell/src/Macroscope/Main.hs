-- |
module Macroscope.Main (runMacroscope) where

import Control.Exception.Safe (tryAny)
import Lentille (MonadLog, MonadRetry)
import Lentille.Bugzilla (BugzillaSession, getApikey, getBZData, getBugzillaSession)
import Lentille.GitHub (GitHubGraphClient, githubDefaultGQLUrl, newGithubGraphClientWithKey)
import Lentille.GitHub.Issues (streamLinkedIssue)
import Lentille.GitLab (GitLabGraphClient, newGitLabGraphClientWithKey)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Macroscope.Worker (DocumentStream (..), runStream)
import qualified Monocle.Api.Config as Config
import Monocle.Client
import Monocle.Prelude

-- | 'MacroM' is an alias for a bunch of constrain.
class (MonadIO m, MonadFail m, MonadMask m, MonadLog m, MonadRetry m) => MacroM m

instance MacroM IO

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
runMacroscope :: MacroM m => Bool -> FilePath -> Word32 -> MonocleClient -> m ()
runMacroscope verbose confPath interval client = do
  monocleLog "Macroscope begin..."
  config <- liftIO $ Config.reloadConfig confPath
  loop config
  where
    loop config = do
      -- Reload config
      conf <- liftIO $ config

      -- Crawl each index
      traverse_ safeCrawl (getCrawlers conf)

      -- Pause
      monocleLog $ "Waiting " <> show (fromIntegral interval_usec / 1_000_000 :: Float) <> "s. brb"
      liftIO $ threadDelay interval_usec

      -- Loop again
      loop config

    interval_usec = fromInteger . toInteger $ interval * 1_000_000

    safeCrawl :: MacroM m => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    safeCrawl crawler = do
      catched <- tryAny $ crawl crawler
      case catched of
        Right comp -> pure comp
        Left exc ->
          let (_, _, Config.Crawler {..}, _) = crawler
           in monocleLog $
                "Skipping crawler: " <> name <> ". Unexpected exception catched: " <> show exc

    crawl :: MacroM m => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    crawl (index, key, crawler, idents) = do
      now <- getCurrentTime
      when verbose (monocleLog $ "Crawling " <> crawlerName crawler)

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

    glMRCrawler :: GitLabGraphClient -> (Text -> Maybe Text) -> DocumentStream
    glMRCrawler glClient cb = Changes $ streamMergeRequests glClient cb

    glOrgCrawler :: GitLabGraphClient -> DocumentStream
    glOrgCrawler glClient = Projects $ streamGroupProjects glClient

    bzCrawler :: BugzillaSession -> Text -> DocumentStream
    bzCrawler bzSession bzProduct = TaskDatas $ getBZData bzSession bzProduct

    ghIssuesCrawler :: GitHubGraphClient -> Text -> DocumentStream
    ghIssuesCrawler ghClient repository =
      TaskDatas $
        streamLinkedIssue ghClient $ toString $ "repo:" <> repository
