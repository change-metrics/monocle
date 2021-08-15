-- |
module Macroscope.Main (runMacroscope) where

import Lentille.GitLab (GitLabGraphClient, newGitLabGraphClientWithKey)
import Lentille.GitLab.Group (streamGroupProjects)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Macroscope.Worker (DocumentStream (..), runStream)
import qualified Monocle.Api.Config as Config
import Monocle.Client
import Monocle.Client.Worker
import Monocle.Prelude

-- | 'MacroM' is an alias for a bunch of constrain.
class (MonadIO m, MonadFail m, MonadMask m, MonadLog m) => MacroM m

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
  reloadableConfig <- Config.loadConfig confPath
  confRef <- newIORef reloadableConfig
  loop confRef
  where
    loop confRef = do
      -- Reload config
      conf <- Config.reloadConfig confRef

      -- Crawl each index
      traverse_ crawl (getCrawlers conf)

      -- Pause
      liftIO $ threadDelay interval_usec

      -- Loop again
      loop confRef

    interval_usec = fromInteger . toInteger $ interval * 1_000_000

    crawl :: MacroM m => (Text, Text, Config.Crawler, [Config.Ident]) -> m ()
    crawl (index, key, crawler, idents) = do
      now <- liftIO getCurrentTime
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
        _ -> pure []

      -- Consume each stream
      let runner =
            runStream client now (toLazy key) (toLazy index) (toLazy $ crawlerName crawler)
      -- TODO: handle exceptions
      traverse_ runner docStreams
      where
        getIdentByAliasCB :: Text -> Maybe Text
        getIdentByAliasCB = flip Config.getIdentByAliasFromIdents idents

    glMRCrawler :: GitLabGraphClient -> (Text -> Maybe Text) -> DocumentStream
    glMRCrawler glClient cb = Changes $ streamMergeRequests glClient cb

    glOrgCrawler :: GitLabGraphClient -> DocumentStream
    glOrgCrawler glClient = Projects $ streamGroupProjects glClient
