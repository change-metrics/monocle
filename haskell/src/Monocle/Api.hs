-- |
module Monocle.Api (run) where

import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Index as I
import Monocle.Client.Worker (MonadLog, retry)
import Monocle.Env
import Monocle.Prelude
import Monocle.Search.Query (loadAliases)
import Monocle.Servant.HTTP (MonocleAPI, server)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant (Handler, hoistServer, serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: AppEnv -> Wai.Application
app env = serve monocleAPI $ hoistServer monocleAPI mkAppM server
  where
    mkAppM :: AppM x -> Handler x
    mkAppM apM = runReaderT (unApp apM) env

run :: (MonadMask m, MonadLog m, MonadIO m) => Int -> Text -> FilePath -> m ()
run port elkUrl configFile = do
  reloadableConfig <- Config.loadConfig configFile
  config <- newIORef reloadableConfig
  let tenants' = Config.configWorkspaces reloadableConfig

  -- Check alias and abort if they are not usable
  case lefts $ map loadAliases tenants' of
    [] -> pure ()
    xs -> do
      liftIO $ traverse_ print (concat xs)
      error $ "Invalid aliases"

  -- TODO: add the aliases to the AppM env to avoid parsing them for each request

  bhEnv <- I.mkEnv elkUrl
  let aEnv = Env {..}
  retry $ liftIO $ traverse_ (\tenant -> runTenantM' bhEnv tenant I.ensureIndex) tenants'
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      putTextLn $
        "Serving " <> show (length tenants') <> " tenant(s) on 0.0.0.0:" <> show port <> " with elk: " <> elkUrl
      Warp.runSettings
        settings
        . cors (const $ Just policy)
        . provideOptions monocleAPI
        $ app (AppEnv {..})
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
