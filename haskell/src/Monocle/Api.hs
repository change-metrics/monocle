-- |
module Monocle.Api (app, run) where

import Lentille (retry)
import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Index as I
import Monocle.Env
import Monocle.Prelude
import Monocle.Search.Query (loadAliases)
import Monocle.Servant.HTTP (MonocleAPI, server)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant (Handler, hoistServer, serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: AppEnv -> Wai.Application
app env = serve monocleAPI $ hoistServer monocleAPI mkAppM server
  where
    mkAppM :: AppM x -> Servant.Handler x
    mkAppM apM = runReaderT (unApp apM) env

healthMiddleware :: Wai.Application -> Wai.Application
healthMiddleware app' req resp
  | Wai.rawPathInfo req == "/health" = resp $ Wai.responseLBS HTTP.status200 mempty "api is running\n"
  | otherwise = app' req resp

run :: Int -> Text -> FilePath -> IO ()
run port url configFile = withLogger (run' port url configFile)

run' :: Int -> Text -> FilePath -> Logger -> IO ()
run' port url configFile glLogger = do
  config <- Config.reloadConfig configFile
  workspaces <- Config.getWorkspaces . snd <$> config

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

  -- Initialize TVar for crawler metadata status by workspaces
  config' <- snd <$> config
  aWSNeedRefresh <- newTVarIO $ (\ws -> (Config.getWorkspaceName ws, False)) <$> Config.getWorkspaces config'

  bhEnv <- mkEnv url
  let aEnv = Env {..}
  retry ("elastic-client", url, "internal") $ liftIO $ traverse_ (\tenant -> runQueryM' bhEnv tenant I.ensureIndex) workspaces
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      logEvent glLogger $ Ready (length workspaces) port url
      Warp.runSettings
        settings
        . cors (const $ Just policy)
        . provideOptions monocleAPI
        . monitoringMiddleware
        . healthMiddleware
        $ app (AppEnv {..})
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
