{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | The Main module of the command line interface.
module CLI (main) where

import Control.Concurrent.CGroup qualified
import Data.Text qualified as T
import Env hiding (Parser, auto, footer)
import Env qualified
import Lentille
import Lentille.Gerrit qualified as G
import Lentille.GitHub.Organization qualified as GH_ORG
import Lentille.GitHub.PullRequests qualified as GH_PR
import Lentille.GitHub.UserPullRequests qualified as GH_UPR
import Lentille.GitHub.Watching qualified
import Lentille.GraphQL (newGraphClient)
import Lentille.Jira qualified as Jira
import Macroscope.Main (runMacroscope)
import Monocle.Backend.Janitor qualified as J
import Monocle.Client (withClient)
import Monocle.Config qualified as Config
import Monocle.Env (mkEnv)
import Monocle.Main (ApiConfig (..))
import Monocle.Main qualified
import Monocle.Prelude hiding ((:::))
import Monocle.Search.Query (parseDateValue)
import Monocle.Version qualified
import Options.Applicative hiding (header, help, str)
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty (string)
import Streaming.Prelude qualified as S

import Data.String.Interpolate (i)
import Effectful.Prometheus
import ListT qualified
import Monocle.Backend.Documents (EChange (..))
import Monocle.Effects
import Monocle.Protob.Change

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
  usageApi = pure do
    -- get parameters from the environment
    ( configFile
      , getURL -> elasticUrl
      , getInt -> port
      , getURL -> publicUrl
      , from -> title
      , webAppPath
      , jwkKey
      , adminToken
      ) <-
      getFromEnv usageApiEnv
    -- start the API
    Monocle.Main.run ApiConfig {..}
  usageApiEnv :: Env.Parser Env.Error (FilePath, String, String, String, String, String, Maybe String, Maybe String)
  usageApiEnv =
    (,,,,,,,)
      <$> envConf
      <*> envElastic
      <*> envApiPort
      <*> envPublicUrl
      <*> envTitle
      <*> envWebAppPath
      <*> envJwkKey
      <*> envAdminToken

  -- The Crawler entrypoint (no CLI argument).
  usageCrawler = pure do
    -- get parameters from the environment
    (config, url, monitoringPort) <- getFromEnv usageCrawlerEnv
    -- start the Crawler
    withClient (from url) Nothing $ runMacroscope (getInt monitoringPort) config
  usageCrawlerEnv = (,,) <$> envConf <*> envPublicUrl <*> envMonitoring

  -- Helper to create sub command
  mkEnvDoc envParser = string (Env.helpDoc envParser)
  mkCommand doc name parser envParser = command name $ info (parser <**> helper) (progDesc doc <> extraHelp)
   where
    -- We only add `--help` to sub command which uses environment
    extraHelp = maybe idm (footerDoc . Just . mkEnvDoc) envParser

  -- Helpers to get value fromm the env
  getFromEnv = Env.parse (header "monocle")
  envConf = var str "MONOCLE_CONFIG" (help "The Monocle configuration" <> envDef "/etc/monocle/config.yaml")
  envElastic = var str "MONOCLE_ELASTIC_URL" (help "The Elasticsearch endpoint URL" <> envDef "http://localhost:9200")
  envApiPort = var str "MONOCLE_API_PORT" (help "The API Port" <> envDef "8080")
  envPublicUrl = var str "MONOCLE_PUBLIC_URL" (help "The Monocle API base URL" <> envDef "http://localhost:8080")
  envTitle = var str "MONOCLE_WEBAPP_TITLE" (help "The Monocle WEB APP title" <> envDef "Monocle")
  envWebAppPath = var str "MONOCLE_WEBAPP_PATH" (help "The Monocle WEB APP build path" <> envDef "/usr/share/monocle/webapp/")
  envJwkKey = optional $ var str "MONOCLE_JWK_GEN_KEY" $ help "The secret key used to issue Authentication tokens (must be 64 characters minimum) (default: None)"
  envAdminToken = optional $ var str "MONOCLE_ADMIN_TOKEN" $ help "Token to access admin endpoints (default: None)"
  envMonitoring = var str "MONOCLE_CRAWLER_MONITORING" (help "The Monitoring Port" <> envDef "9001")
  getInt txt = fromMaybe (error . from $ "Invalid number: " <> txt) $ readMaybe txt

  envDef :: String -> Env.Mod Var String
  envDef s = def s <> helpDef show

-- | The CLI entrypoint.
main :: IO ()
main = do
  Control.Concurrent.CGroup.initRTSThreads
  withOpenSSL . join $ execParser opts
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
   in from url'

mkSubCommand :: String -> String -> Parser a -> O.Mod CommandFields a
mkSubCommand name doc parser = command name $ info (parser <**> helper) $ progDesc doc

---------------------------------------------------------------
-- Janitor cli
---------------------------------------------------------------
usageJanitor :: Parser (IO ())
usageJanitor =
  subparser
    ( mkSubCommand "update-idents" "Update author identities" janitorUpdateIdent
        <> mkSubCommand "wipe-crawler-data" "Remove changes/task-data and events related to a crawler name" janitorRemoveCrawlerData
        <> mkSubCommand "set-crawler-commit-date" "Overwrite the crawler commit date" janitorSetCrawlerCommitDate
    )
 where
  configOption = strOption (long "config" <> O.help "Path to configuration file" <> metavar "MONOCLE_CONFIG")
  elasticOption = strOption (long "elastic" <> O.help "The Elastic endpoint url" <> metavar "MONOCLE_ELASTIC_URL")
  workspaceOption =
    option
      (eitherReader $ (first T.unpack . Config.mkIndexName) . T.pack)
      (long "workspace" <> O.help "Workspace name" <> metavar "WORKSPACE")
  crawlerNameOption = strOption (long "crawler-name" <> O.help "The crawler name" <> metavar "CRAWLER_NAME")
  runOnWorkspace env action' workspace = runEff $ runLoggerEffect $ runElasticEffect env $ runEmptyQueryM workspace $ dieOnEsError action'
  noWorkspace workspaceName = "Unable to find the workspace " <> Config.getIndexName workspaceName <> " in the Monocle config"
  janitorUpdateIdent = io <$> parser
   where
    parser = (,,) <$> configOption <*> elasticOption <*> optional workspaceOption
    io (configPath, elasticUrl, workspaceNameM) = do
      config <- Config.loadConfigWithoutEnv configPath
      env <- mkEnv $ getURL elasticUrl
      case workspaceNameM of
        Just workspaceName -> do
          void $ case Config.lookupTenant (Config.getWorkspaces config) workspaceName of
            Nothing -> print $ noWorkspace workspaceName
            Just workspace -> runOnWorkspace env J.updateIdentsOnWorkspace workspace
        Nothing -> traverse_ (runOnWorkspace env J.updateIdentsOnWorkspace) $ Config.getWorkspaces config
  janitorRemoveCrawlerData = io <$> parser
   where
    parser = (,,,) <$> configOption <*> elasticOption <*> workspaceOption <*> crawlerNameOption
    io (configPath, elasticUrl, workspaceName, crawlerName) = do
      config <- Config.loadConfigWithoutEnv configPath
      env <- mkEnv $ getURL elasticUrl
      void $ case Config.lookupTenant (Config.getWorkspaces config) workspaceName of
        Nothing -> print $ noWorkspace workspaceName
        Just workspace -> do
          runOnWorkspace env (J.wipeCrawlerData crawlerName) workspace
          runOnWorkspace env (J.removeTDCrawlerData crawlerName) workspace
  janitorSetCrawlerCommitDate = io <$> parser
   where
    newDateOption = strOption (long "commit-date" <> O.help "The new crawler commit-date" <> metavar "COMMIT_DATE")
    parser = (,,,,) <$> configOption <*> elasticOption <*> workspaceOption <*> crawlerNameOption <*> newDateOption
    io (configPath, elasticUrl, workspaceName, crawlerName, newDate) = do
      config <- Config.loadConfigWithoutEnv configPath
      env <- mkEnv $ getURL elasticUrl
      void $ case Config.lookupTenant (Config.getWorkspaces config) workspaceName of
        Nothing -> print $ noWorkspace workspaceName
        Just workspace ->
          runOnWorkspace
            env
            ( J.updateCrawlerMDLastUpdatedDate workspace crawlerName newDate
            )
            workspace

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
        <> mkSubCommand "github-user-changes" "Get changes list" githubUserChangesUsage
        <> mkSubCommand "github-watching" "Get watched list" githubWatchingUsage
        <> mkSubCommand "jira-issues" "Get recent issues" jiraIssuesUsage
    )
 where
  gerritChangeUsage = io <$> parser
   where
    parser = (,) <$> urlOption <*> changeOption
    io (url, change) = runStandaloneStream do
      env <- getGerritEnv url
      dumpJSON Nothing $ G.streamChange env [G.ChangeId $ show (change :: Int)]

  gerritProjectsUsage = io <$> parser
   where
    parser = (,) <$> urlOption <*> queryOption
    io (url, query) = runStandaloneStream do
      env <- getGerritEnv url
      dumpJSON Nothing $ G.streamProject env $ G.Regexp query

  gerritChangesUsage = io <$> parser
   where
    parser = (,,,) <$> urlOption <*> projectOption <*> sinceOption <*> limitOption
    io (url, project, since, limit) = runStandaloneStream do
      env <- getGerritEnv url
      dumpJSON limit $ G.streamChange env [G.Project project, G.After (toSince since)]

  githubProjectsUsage = io <$> parser
   where
    parser = (,,) <$> urlOption <*> secretOption <*> orgOption
    io (url, secret, org) = runStandaloneStream do
      client <- getGraphClient url secret
      dumpJSON Nothing $ GH_ORG.streamOrganizationProjects client org

  githubWatchingUsage = io <$> parser
   where
    parser = (,,,) <$> urlOption <*> secretOption <*> userOption <*> limitOption
    io (url, secret, user, limitM) = runStandaloneStream do
      client <- getGraphClient url secret
      dumpJSON limitM $ Lentille.GitHub.Watching.streamWatchedProjects client user

  githubChangesUsage = io <$> parser
   where
    parser =
      (,,,,,)
        <$> urlOption
        <*> secretOption
        <*> projectOption
        <*> sinceOption
        <*> limitOption
        <*> humanOption
    io (url, secret, repo, since, limitM, humanF) = runStandaloneStream do
      client <- getGraphClient url secret
      let stream = GH_PR.streamPullRequests client (const Nothing) (toSince since) repo
      if humanF
        then dumpChanges formatChange limitM stream
        else dumpJSON limitM stream

  githubUserChangesUsage = io <$> parser
   where
    parser =
      (,,,,,)
        <$> urlOption
        <*> secretOption
        <*> userOption
        <*> sinceOption
        <*> limitOption
        <*> humanOption
    io (url, secret, user, since, limitM, humanF) = runStandaloneStream do
      client <- getGraphClient url secret
      let stream = GH_UPR.streamUserPullRequests client (const Nothing) (toSince since) user
      if humanF
        then dumpChanges formatChange limitM stream
        else dumpJSON limitM stream

  jiraIssuesUsage = io <$> parser
   where
    parser = (,,,) <$> urlOption <*> secretOption <*> sinceOption <*> jqlOption
    io (url, secret, since, jql) = runStandaloneStream do
      let client = getJiraClient url secret
      dumpListT $ Jira.getIssueBodies client (toSince since) jql

  toSince txt = case Monocle.Search.Query.parseDateValue txt of
    Just x -> x
    Nothing -> error $ "Invalid date: " <> show txt
  getGerritEnv url = do
    client <- G.getGerritClient url Nothing
    pure $ G.GerritEnv client Nothing (const Nothing) "cli"
  getGraphClient url secret = newGraphClient url (from secret)
  getJiraClient url token = Jira.newJiraClient url (from token)

  urlOption = strOption (long "url" <> O.help "API url, e.g. https://api.github.com/graphql" <> metavar "URL")
  jqlOption = Jira.JQL <$> strOption (long "jql" <> O.help "Jira query")
  queryOption = strOption (long "query" <> O.help "Gerrit regexp query")
  orgOption = strOption (long "organization" <> O.help "GitHub organization name")
  userOption = strOption (long "user" <> O.help "GitHub user name")
  secretOption = into @Secret @Text <$> strOption (long "token" <> O.help "GitHub token, get one from https://github.com/settings/tokens")
  changeOption = option auto (long "change" <> O.help "Change Number" <> metavar "NR")
  projectOption = strOption (long "project" <> O.help "Project name")
  sinceOption = strOption (long "since" <> O.help "Since date")
  limitOption = optional $ option auto (long "limit" <> O.help "Limit count")
  humanOption = Options.Applicative.flag False True (long "human" <> O.help "Human readable display")

  formatChange :: (Change, [ChangeEvent]) -> String
  formatChange (c, _) = let (EChange {..}) = from c in [i|#{echangeUrl}\t#{echangeTitle}\t#{echangeUpdatedAt}|]

  runStandaloneStream = runEff . runErrorIO @LentilleError . runConcurrent . runFailIO . runLoggerEffect . runTime . runRetry . runHttpEffect . G.runGerrit . runPrometheus

dumpJSON :: (IOE :> es, ToJSON a) => Maybe Int -> Stream (Of a) (Eff es) () -> Eff es ()
dumpJSON limitM stream = do
  xsE <- tryAny $ S.toList_ $ brk stream
  case xsE of
    Right xs -> liftIO . putBSLn . from . encodePrettyWithSpace 2 $ xs
    Left err -> liftIO . putBSLn $ "Couldn't evalue the stream due to " <> show err
  liftIO . putLBSLn =<< exportMetricsAsText
 where
  brk = maybe id S.take limitM

dumpChanges ::
  IOE :> es =>
  (Changes -> String) ->
  Maybe Int ->
  Stream (Of (Either LentilleError Changes)) (Eff es) () ->
  Eff es ()
dumpChanges format limitM stream = do
  xsE <- tryAny $ S.toList_ $ brk stream
  case xsE of
    Right xs -> liftIO $ showChanges xs
    Left err -> liftIO . putBSLn $ "Couldn't evalue the stream due to " <> show err
 where
  brk = maybe id S.take limitM
  showChanges :: [Either LentilleError Changes] -> IO ()
  showChanges =
    mapM_
      ( \case
          Right change -> putStrLn $ format change
          Left err -> putStrLn $ "LentilleError : " <> show err
      )

dumpListT :: (IOE :> es, ToJSON a) => ListT (Eff es) a -> Eff es ()
dumpListT listT = do
  ListT.uncons listT >>= \case
    Nothing -> pure ()
    Just (x, xs) -> do
      liftIO . putBSLn . from . encodePrettyWithSpace 2 $ x
      dumpListT xs
