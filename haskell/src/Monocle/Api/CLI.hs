{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.CLI (run) where

import qualified Monocle.Api.Config as Config
import qualified Monocle.Search.Queries as Q
import Monocle.Servant.Env
import Monocle.Servant.HTTP (MonocleAPI, server)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Relude
import Servant (hoistServer, serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: Env -> Wai.Application
app env = serve monocleAPI $ hoistServer monocleAPI (`runReaderT` env) server

run :: MonadIO m => Int -> Text -> FilePath -> m ()
run port elkUrl configFile = do
  tenants' <- Config.loadConfig configFile
  bhEnv' <- Q.mkEnv elkUrl
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
