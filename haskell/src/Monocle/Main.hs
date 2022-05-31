-- | The Monocle entry point.
module Monocle.Main (run, app) where

import Lentille (httpRetry)
import Monocle.Api.Jwt qualified as Monocle.Api.JWK
import Monocle.Backend.Index qualified as I
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
import Servant (Context (EmptyContext, (:.)), Handler, hoistServerWithContext, serveWithContext)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

-- | Create the underlying Monocle web application interface, for integration or testing purpose.
app :: AppEnv -> Wai.Application
-- app env = serve monocleAPI $ hoistServer monocleAPI mkAppM server
app env =
  serveWithContext monocleAPI cfg $
    hoistServerWithContext monocleAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) mkAppM server
  where
    jwtCfg = aJWTSettings env
    cfg = jwtCfg :. defaultCookieSettings :. EmptyContext
    mkAppM :: AppM x -> Servant.Handler x
    mkAppM apM = runReaderT (unApp apM) env

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

  -- Monitoring
  void $ register ghcMetrics
  let monitoringMiddleware = prometheus def

  -- Initialize workspace status to ready since we are starting
  wsRef <- Config.csWorkspaceStatus <$> config
  Config.setWorkspaceStatus Config.Ready wsRef

  -- Generate random JWK and JWTSettings
  jwk <- Monocle.Api.JWK.doGenJwk
  let aJWTSettings = defaultJWTSettings jwk

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
        $ app (AppEnv {..})
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
