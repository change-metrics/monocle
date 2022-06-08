-- | The Monocle entry point.
module Monocle.Main (run, app) where

import Data.List qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Lentille (httpRetry)
import Monocle.Api.Jwt (LoginInUser (..), doGenJwk, initOIDCEnv)
import Monocle.Api.Server (handleLoggedIn, handleLogin)
import Monocle.Backend.Index qualified as I
import Monocle.Config (getAuthProvider)
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Prelude
import Monocle.Search.Query (loadAliases)
import Monocle.Servant.HTTP (MonocleAPI, server)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)
import Servant.HTML.Blaze (HTML)
import System.Directory qualified

-- | The API is served at both `/api/2/` (for backward compat with the legacy nginx proxy)
-- and `/` (for compat with crawler client)
type RootAPI = "api" :> "2" :> MonocleAPI :<|> MonocleAPI :<|> AuthAPI

type AuthAPI =
  "auth" :> "login" :> Get '[JSON] NoContent
    :<|> "auth" :> "cb" :> QueryParam "error" Text :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[HTML] LoginInUser

serverAuth :: ServerT AuthAPI AppM
serverAuth = handleLogin :<|> handleLoggedIn

-- | Create the underlying Monocle web application interface, for integration or testing purpose.
app :: AppEnv -> Wai.Application
app env = do
  serveWithContext (Proxy @RootAPI) cfg $
    hoistServerWithContext
      (Proxy @RootAPI)
      (Proxy :: Proxy '[CookieSettings, JWTSettings])
      mkAppM
      (server :<|> server :<|> serverAuth)
  where
    jwtCfg = localJWTSettings $ aOIDC env
    cfg = jwtCfg :. defaultCookieSettings :. EmptyContext
    mkAppM :: AppM x -> Servant.Handler x
    mkAppM apM = runReaderT (unApp apM) env

mkStaticMiddleware :: IO (Wai.Application -> Wai.Application)
mkStaticMiddleware = do
  -- Check where are the webui files
  rootDir <- fromMaybe (error "WebUI files are missing") <$> "/usr/share/monocle/webapp/" `existOr` Just "../web/build/"
  -- Load the index and inject the customization
  index <- Text.readFile $ rootDir <> "index.html"
  title <- fromMaybe "Monocle" <$> lookupEnv "REACT_APP_TITLE"
  apiUrl <- lookupEnv "REACT_APP_API_URL"
  pure $ staticMiddleware (from $ prepIndex index title apiUrl) rootDir
  where
    -- Replace env variable in the index page
    prepIndex :: Text -> String -> Maybe String -> Text
    prepIndex index title apiUrl =
      Text.replace "__TITLE__" (from title) $
        case apiUrl of
          Just url -> Text.replace "__API_URL__" (from url) index
          Nothing -> index

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

-- | Start the API in the foreground.
run :: Int -> Text -> FilePath -> IO ()
run port url configFile = withLogger (run' port url configFile)

run' :: Int -> Text -> FilePath -> Logger -> IO ()
run' port url configFile glLogger = do
  config <- Config.reloadConfig configFile
  conf <- Config.csConfig <$> config
  let workspaces = Config.getWorkspaces conf

  -- Check alias and abort if they are not usable
  case lefts $ map loadAliases workspaces of
    [] -> pure ()
    xs -> do
      liftIO $ traverse_ print (concat xs)
      error "Invalid aliases"

  -- TODO: add the aliases to the AppM env to avoid parsing them for each request

  staticMiddleware <- mkStaticMiddleware

  -- Monitoring
  void $ register ghcMetrics
  let monitoringMiddleware = prometheus def

  -- Initialize workspace status to ready since we are starting
  wsRef <- Config.csWorkspaceStatus <$> config
  Config.setWorkspaceStatus Config.Ready wsRef

  -- Init OIDC
  -- Initialise JWT settings for locally issuing JWT (local provider)
  localJwk <- doGenJwk
  providerM <- getAuthProvider
  let localJWTSettings = defaultJWTSettings localJwk
  -- Initialize env to talk with OIDC provider
  oidcEnv <- case providerM of
    Just provider -> pure <$> initOIDCEnv provider
    _ -> pure Nothing
  let aOIDC = OIDC {..}

  bhEnv <- mkEnv url
  let aEnv = Env {..}
  httpRetry ("elastic-client", url, "internal") $
    liftIO $ traverse_ (\tenant -> runQueryM' bhEnv tenant I.ensureIndex) workspaces
  httpRetry ("elastic-client", url, "internal") $
    liftIO $ runQueryTarget bhEnv (QueryConfig conf) I.ensureConfigIndex
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      doLog glLogger $ via @Text $ SystemReady (length workspaces) port url
      Warp.runSettings
        settings
        . cors (const $ Just policy)
        . monitoringMiddleware
        . healthMiddleware
        . staticMiddleware
        $ app (AppEnv {..})
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
