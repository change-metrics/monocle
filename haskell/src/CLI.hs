{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | The Main module of the command line interface.
module CLI (main) where

import Env hiding (Parser, auto, footer)
import Env qualified
import Lentille.Gerrit qualified as G
import Lentille.GitHub.Organization qualified as GH_ORG
import Lentille.GitHub.PullRequests qualified as GH_PR
import Lentille.GitHub.Watching qualified
import Lentille.GraphQL (newGraphClient)
import Macroscope.Main (runMacroscope)
import Monocle.Backend.Janitor qualified as J
import Monocle.Client (withClient)
import Monocle.Config qualified as Config
import Monocle.Env (mkEnv, runQueryM')
import Monocle.Logging (LogCrawlerContext (..))
import Monocle.Main qualified
import Monocle.Prelude hiding ((:::))
import Monocle.Search.Query (parseDateValue)
import Monocle.Version qualified
import Options.Applicative hiding (header, help, str)
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty (string)
import Streaming.Prelude qualified as S

---------------------------------------------------------------
-- Unified CLI
---------------------------------------------------------------

-- | Usage is a command line parser that returns the CLI action as a `IO ()` value.
-- See the last example of https://github.com/pcapriotti/optparse-applicative#commands
usage :: Options.Applicative.Parser (IO ())
usage =
  subparser
    ( mkCommand "Start the API" "api" usageApi (Just usageApiEnv)
        <> mkCommand "Start the Crawlers" "crawler" usageCrawler (Just usageCrawlerEnv)
        <> mkCommand "Maintain the database" "janitor" usageJanitor Nothing
        <> mkCommand "Run a single crawler standlone" "lentille" usageLentille Nothing
    )
  where
    -- The API entrypoint (no CLI argument).
    usageApi :: O.Parser (IO ())
    usageApi = pure $ do
      -- get parameters from the environment
      (config, elastic, port) <- getFromEnv usageApiEnv
      -- start the API
      Monocle.Main.run (getInt port) (getURL elastic) config
    usageApiEnv :: Env.Parser Env.Error (FilePath, String, String)
    usageApiEnv = (,,) <$> envConf <*> envElastic <*> envApiPort

    -- The Crawler entrypoint (no CLI argument).
    usageCrawler = pure $ do
      -- get parameters from the environment
      (config, url, monitoringPort) <- getFromEnv usageCrawlerEnv
      -- start the Crawler
      withClient url Nothing Nothing $ runMacroscope (getInt monitoringPort) config
    usageCrawlerEnv = (,,) <$> envConf <*> envPublicUrl <*> envMonitoring

    -- Helper to create sub command
    mkEnvDoc envParser = string (Env.helpDoc envParser)
    mkCommand doc name parser envParser = command name $ info (parser <**> helper) (progDesc doc <> extraHelp)
      where
        -- We only add `--help` to sub command which uses environment
        extraHelp = maybe idm (footerDoc . Just . mkEnvDoc) envParser

    -- Helpers to get value fromm the env
    getFromEnv = Env.parse (header "monocle")
    envConf = var str "CONFIG" (help "The Monocle configuration" <> def "/etc/monocle/config.yaml" <> helpDef show)
    envElastic = var str "ELASTIC_CONN" (help "The Elasticsearch endpoint" <> def "elastic:9200")
    envApiPort = var str "MONOCLE_API_PORT" (help "The API Port" <> def "9898")
    envPublicUrl = var str "MONOCLE_PUBLIC_URL" (help "The Monocle URL" <> def "http://web:8080")
    envMonitoring = var str "MONOCLE_CRAWLER_MONITORING" (help "The Monitoring Port" <> def "9001")
    getInt txt = fromMaybe (error . from $ "Invalid number: " <> txt) $ readMaybe txt

-- | The CLI entrypoint.
main :: IO ()
main = withOpenSSL $ join $ execParser opts
  where
    opts =
      info
        (versionOption <*> usage <**> helper)
        (fullDesc <> progDesc "changemetrics.io | monocle")
    versionOption = infoOption ("monocle " <> Monocle.Version.version) (long "version" <> O.help "Show version")

getURL :: String -> Text
getURL url =
  let hasScheme = isPrefixOf "http://" url || isPrefixOf "https://" url
      url' = if hasScheme then url else "http://" <> url
   in toText url'

mkSubCommand :: String -> String -> Parser a -> O.Mod CommandFields a
mkSubCommand name doc parser = command name $ info (parser <**> helper) $ progDesc doc

---------------------------------------------------------------
-- Janitor cli
---------------------------------------------------------------
usageJanitor :: Parser (IO ())
usageJanitor =
  subparser
    ( mkSubCommand "update-idents" "Update author identities" janitorUpdateIdent
    )
  where
    janitorUpdateIdent = io <$> parser
      where
        parser = (,,) <$> configOption <*> elasticOption <*> workspaceOption
        io (configPath, elasticUrl, workspaceNameM) = do
          config <- Config.loadConfigWithoutEnv configPath
          env <- mkEnv $ getURL elasticUrl
          case workspaceNameM of
            Just workspaceName -> do
              void $ case Config.lookupTenant (Config.getWorkspaces config) workspaceName of
                Nothing -> print $ "Unable to find the workspace " <> workspaceName <> " in the Monocle config"
                Just workspace -> runOnWorkspace env workspace
            Nothing -> traverse_ (runOnWorkspace env) $ Config.getWorkspaces config
        workspaceOption = optional $ strOption (long "workspace" <> O.help "Workspace name" <> metavar "WORKSPACE")
        configOption = strOption (long "config" <> O.help "Path to configuration file" <> metavar "CONFIG")
        elasticOption = strOption (long "elastic" <> O.help "The Elastic endpoint url" <> metavar "ELASTIC_URL")
        runOnWorkspace env workspace = runQueryM' env workspace J.updateIdentsOnWorkspace

---------------------------------------------------------------
-- Lentille cli
---------------------------------------------------------------
usageLentille :: Parser (IO ())
usageLentille =
  subparser
    ( mkSubCommand "gerrit-change" "Get a single change" gerritChangeUsage
        <> mkSubCommand "gerrit-projects" "Get projects list" gerritProjectsUsage
        <> mkSubCommand "gerrit-changes" "Get changes list" gerritChangesUsage
        <> mkSubCommand "github-projects" "Get projects list" githubProjectsUsage
        <> mkSubCommand "github-changes" "Get changes list" githubChangesUsage
        <> mkSubCommand "github-watching" "Get watched list" githubWatchingUsage
    )
  where
    gerritChangeUsage = io <$> parser
      where
        parser = (,) <$> urlOption <*> changeOption
        io (url, change) = do
          env <- getGerritEnv url
          dump Nothing $ G.streamChange env [G.ChangeId $ show (change :: Int)]

    gerritProjectsUsage = io <$> parser
      where
        parser = (,) <$> urlOption <*> queryOption
        io (url, query) = do
          env <- getGerritEnv url
          dump Nothing $ G.streamProject env $ G.Regexp query

    gerritChangesUsage = io <$> parser
      where
        parser = (,,,) <$> urlOption <*> projectOption <*> sinceOption <*> limitOption
        io (url, project, since, limit) = do
          env <- getGerritEnv url
          dump limit $ G.streamChange env [G.Project project, G.After (toSince since)]

    githubProjectsUsage = io <$> parser
      where
        parser = (,,) <$> urlOption <*> secretOption <*> orgOption
        io (url, secret, org) = do
          client <- getGraphClient url secret
          dump Nothing $ GH_ORG.streamOrganizationProjects client mkLC org

    githubWatchingUsage = io <$> parser
      where
        parser = (,,,) <$> urlOption <*> secretOption <*> userOption <*> limitOption
        io (url, secret, user, limitM) = do
          client <- getGraphClient url secret
          dump limitM $ Lentille.GitHub.Watching.streamWatchedProjects client mkLC user

    githubChangesUsage = io <$> parser
      where
        parser =
          (,,,,)
            <$> urlOption
            <*> secretOption
            <*> projectOption
            <*> sinceOption
            <*> limitOption
        io (url, secret, repo, since, limitM) = do
          client <- getGraphClient url secret
          dump limitM $ GH_PR.streamPullRequests client mkLC (const Nothing) (toSince since) repo

    toSince txt = case Monocle.Search.Query.parseDateValue txt of
      Just x -> x
      Nothing -> error $ "Invalid date: " <> show txt
    getGerritEnv url = do
      client <- G.getGerritClient url Nothing
      pure $ G.GerritEnv client Nothing (const Nothing) "cli"
    getGraphClient url secret = newGraphClient url (Secret secret)

    mkLC = LogCrawlerContext "<stdout>" "cli" . Just

    urlOption = strOption (long "url" <> O.help "API url, e.g. https://api.github.com/graphql" <> metavar "URL")
    queryOption = strOption (long "query" <> O.help "Gerrit regexp query")
    orgOption = strOption (long "organization" <> O.help "GitHub organization name")
    userOption = strOption (long "user" <> O.help "GitHub user name")
    secretOption = strOption (long "token" <> O.help "GitHub token, get one from https://github.com/settings/tokens")
    changeOption = option auto (long "change" <> O.help "Change Number" <> metavar "NR")
    projectOption = strOption (long "project" <> O.help "Project name")
    sinceOption = strOption (long "since" <> O.help "Since date")
    limitOption = optional $ option auto (long "limit" <> O.help "Limit count")

dump :: (MonadCatch m, MonadIO m, ToJSON a) => Maybe Int -> Stream (Of a) m () -> m ()
dump limitM stream = do
  xsE <- tryAny $ S.toList_ $ brk stream
  case xsE of
    Right xs -> liftIO . putBSLn . from . encodePrettyWithSpace 2 $ xs
    Left err -> liftIO . putBSLn $ "Couldn't evalue the stream due to " <> show err
  liftIO . putLBSLn =<< exportMetricsAsText
  where
    brk = maybe id S.take limitM
