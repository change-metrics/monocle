{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.CLI (run) where

import qualified Database.Bloodhound as BH
import Monocle.Api.Client.Worker (MonadLog, retry)
import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Index as I
import Monocle.Prelude
import Monocle.Servant.Env
import Monocle.Servant.HTTP (MonocleAPI, server)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant (Handler, hoistServer, serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: Env -> Wai.Application
app env = serve monocleAPI $ hoistServer monocleAPI mkAppM server
  where
    mkAppM :: AppM x -> Handler x
    mkAppM apM = runReaderT (unApp apM) env

run :: (MonadMask m, MonadLog m, MonadIO m, MonadFail m) => Int -> Text -> FilePath -> m ()
run port elkUrl configFile = do
  tenants' <- getExn <$> Config.loadConfig configFile
  bhEnv' <- I.mkEnv elkUrl
  retry $ BH.runBH bhEnv' $ traverse_ I.ensureIndex tenants'
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      putTextLn $
        "Serving " <> show (length tenants') <> " tenant(s) on 0.0.0.0:" <> show port <> " with elk: " <> elkUrl
      Warp.runSettings
        settings
        . cors (const $ Just policy)
        . provideOptions monocleAPI
        $ app (Env tenants' bhEnv')
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
