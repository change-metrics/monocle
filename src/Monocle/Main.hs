-- This is for https://github.com/Kleidukos/servant-effectful/pull/4
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The Monocle entry point.
module Monocle.Main (run, withApp, rootServer, ApiConfig (..), defaultApiConfig, RootAPI) where

import Data.List qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Lentille (httpRetry)
import Monocle.Api.Jwt (LoginInUser (..), doGenJwk, initOIDCEnv)
import Monocle.Api.Server (handleLoggedIn, handleLogin)
import Monocle.Backend.Index qualified as I
import Monocle.Config (getAuthProvider, opName)
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Prelude
import Monocle.Search.Query (loadAliases)
import Monocle.Servant.HTTP (MonocleAPI, server)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)
import Servant.HTML.Blaze (HTML)
import System.Directory qualified

import Monocle.Effects

import Effectful qualified as E
import Effectful.Error.Static qualified as E
import Effectful.Fail qualified as E
import Effectful.Reader.Static qualified as E
import Effectful.Servant qualified as ES

runWarpServerSettingsContext ::
  forall (api :: Type) (context :: [Type]) (es :: [E.Effect]).
  (HasServer api context, ServerContext context, [IOE, E.Error ServerError] :>> es) =>
  Warp.Settings ->
  Context context ->
  ES.ServerEff api es ->
  Wai.Middleware ->
  Eff es ()
runWarpServerSettingsContext settings cfg serverEff middleware = do
  E.withEffToIO $ \runInIO -> do
    let api = Proxy @api
        ctx = Proxy @context
        server' = Servant.hoistServerWithContext @api @context api ctx (ES.effToHandlerWith runInIO) serverEff
    Warp.runSettings settings (middleware (Servant.serveWithContext api cfg $ server'))

withApp ::
  forall (api :: Type) (context :: [Type]) (es :: [E.Effect]).
  (HasServer api context, ServerContext context, [IOE, E.Error ServerError] :>> es) =>
  Context context ->
  ES.ServerEff api es ->
  (Wai.Application -> IO ()) ->
  Eff es ()
withApp cfg serverEff cb = do
  E.withEffToIO $ \runInIO -> do
    let api = Proxy @api
        ctx = Proxy @context
        server' = Servant.hoistServerWithContext @api @context api ctx (ES.effToHandlerWith runInIO) serverEff
    cb (Servant.serveWithContext api cfg server')

-- | The API is served at both `/api/2/` (for backward compat with the legacy nginx proxy)
-- and `/` (for compat with crawler client)
type MonocleAPI' = MonocleAPI :<|> AuthAPI

type RootAPI = "api" :> "2" :> MonocleAPI' :<|> MonocleAPI'

type AuthAPI =
  "auth" :> "login" :> QueryParam "redirectUri" Text :> Get '[JSON] NoContent
    :<|> "auth" :> "cb" :> QueryParam "error" Text :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[HTML] LoginInUser

rootServer :: ApiEffects es => ES.ServerEff RootAPI es
rootServer = app :<|> app
 where
  app = undefined :<|> handleLogin :<|> handleLoggedIn

fallbackWebAppPath :: FilePath
fallbackWebAppPath = "web/build/"

mkStaticMiddleware :: Text -> Text -> FilePath -> IO (Wai.Application -> Wai.Application)
mkStaticMiddleware publicUrl title webAppPath = do
  -- Check where are the webui files
  rootDir <- fromMaybe (error "Web APP files are missing") <$> webAppPath `existOr` Just fallbackWebAppPath
  -- Load the index and inject the customization
  index <- Text.readFile $ rootDir <> "index.html"
  pure $ staticMiddleware (from $ prepIndex index) rootDir
 where
  -- Replace env variable in the index page
  prepIndex :: Text -> Text
  prepIndex index = Text.replace "__TITLE__" (from title) $ Text.replace "__API_URL__" (from publicUrl) index

  -- Helper that checks if `fp` exists, otherwise it returns `otherFP`
  existOr :: FilePath -> Maybe FilePath -> IO (Maybe FilePath)
  existOr fp otherFP = do
    exist <- System.Directory.doesPathExist fp
    pure $ if exist then Just fp else otherFP

  -- The middleware pass the request to the monocle app
  staticMiddleware :: LByteString -> FilePath -> Wai.Application -> Wai.Application
  staticMiddleware index rootDir app' req waiRespond = app' req responder
   where
    responder resp
      -- The application handled the request, forward the responce
      | HTTP.statusCode (Wai.responseStatus resp) /= 404 = waiRespond resp
      | otherwise = handle
    handle = do
      respPath <- do
        let reqPath = drop 1 $ decodeUtf8 $ Wai.rawPathInfo req
        if Data.List.null reqPath || ".." `Data.List.isInfixOf` reqPath
          then -- The path is empty or fishy
            pure Nothing
          else -- Checks if the request match a file, such as favico or css
            (rootDir <> reqPath) `existOr` Nothing
      waiRespond $ case respPath of
        -- The path exist, returns it
        Just path -> Wai.responseFile HTTP.status200 [] path Nothing
        -- Otherwise returns the index
        Nothing -> Wai.responseLBS HTTP.status200 [] index

healthMiddleware :: Wai.Application -> Wai.Application
healthMiddleware app' req resp
  | Wai.rawPathInfo req == "/health" = resp $ Wai.responseLBS HTTP.status200 mempty "api is running\n"
  | otherwise = app' req resp

data ApiConfig = ApiConfig
  { port :: Int
  , elasticUrl :: Text
  , configFile :: FilePath
  , publicUrl :: Text
  , title :: Text
  , webAppPath :: FilePath
  , jwkKey :: Maybe String
  , adminToken :: Maybe String
  }

defaultApiConfig :: Int -> Text -> FilePath -> ApiConfig
defaultApiConfig port elasticUrl configFile =
  let publicUrl = "http://localhost:" <> show port
      title = "Monocle"
      jwkKey = Nothing
      adminToken = Nothing
      webAppPath = fallbackWebAppPath
   in ApiConfig {..}

-- | Start the API in the foreground.
run :: ApiConfig -> IO ()
run cfg =
  withStdoutLogger $ \aplogger ->
    withLogger $ \logger ->
      runEff
        . runMonoConfig (configFile cfg)
        $ run' cfg aplogger logger

run' :: '[IOE, MonoConfigEffect] :>> es => ApiConfig -> ApacheLogger -> Logger -> Eff es ()
run' ApiConfig {..} aplogger glLogger = runLoggerEffect do
  conf <- Config.csConfig <$> getReloadConfig
  let workspaces = Config.getWorkspaces conf

  -- Check alias and abort if they are not usable
  case lefts $ map loadAliases workspaces of
    [] -> pure ()
    xs -> do
      liftIO $ traverse_ print (concat xs)
      error "Invalid aliases"

  -- TODO: add the aliases to the AppM env to avoid parsing them for each request

  staticMiddleware <- liftIO (mkStaticMiddleware publicUrl title webAppPath)

  -- Monitoring
  void $ register ghcMetrics
  let monitoringMiddleware = prometheus def

  -- Initialize workspace status to ready since we are starting
  wsRef <- Config.csWorkspaceStatus <$> getReloadConfig
  liftIO (Config.setWorkspaceStatus Config.Ready wsRef)

  -- Init OIDC
  -- Initialise JWT settings for locally issuing JWT (local provider)
  localJwk <- liftIO . doGenJwk $ from <$> jwkKey
  providerM <- liftIO (getAuthProvider publicUrl conf)
  let localJWTSettings = defaultJWTSettings localJwk
  -- Initialize env to talk with OIDC provider
  oidcEnv <- case providerM of
    Just provider -> do
      runLogger glLogger $ logInfo "AuthSystemReady" ["provider" .= opName provider]
      pure <$> liftIO (initOIDCEnv provider)
    _ -> pure Nothing
  let aOIDC = OIDC {..}

  bhEnv <- mkEnv elasticUrl
  let aEnv = Env {..}
  r <- E.runFail $ runElasticEffect bhEnv do
    traverse_ (\workspace -> runEmptyMonoQuery workspace I.ensureIndex) workspaces
    runMonoQuery (MonoQueryEnv (QueryConfig conf) (mkQuery [])) I.ensureConfigIndex

    let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
        jwtCfg = localJWTSettings
        cfg = jwtCfg :. defaultCookieSettings :. EmptyContext
        middleware =
          ( cors (const $ Just policy)
              . monitoringMiddleware
              . healthMiddleware
              . staticMiddleware
          )
    logInfo' "SystemReady" ["workspace" .= length workspaces, "port" .= port, "elastic" .= elasticUrl]

    appEnv <- E.withEffToIO $ \effToIO -> do
      let configIO = effToIO getReloadConfig
      pure AppEnv {aEnv, aOIDC, config = configIO}

    r <-
      -- it's a bit weird we need to runError here, it should only be necessary for the server
      E.runErrorNoCallStack $
        E.runReader appEnv $
          runWarpServerSettingsContext @RootAPI settings cfg rootServer middleware
    case r of
      Left (x :: ServerError) -> error (show x)
      Right () -> pure ()
  case r of
    Left e -> error (show e)
    Right () -> pure ()
 where
  policy =
    simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
