{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.CLI (run) where

import qualified Monocle.Api.Config as Config
import Monocle.Api.HTTP (MonocleAPI, server)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Relude
import Servant (serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: [Config.Tenant] -> Wai.Application
app tenants = serve monocleAPI (server tenants)

run :: MonadIO m => Int -> FilePath -> m ()
run port configFile = do
  tenants <- Config.loadConfig configFile
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      putTextLn $ "Serving " <> show (length tenants) <> " tenant(s) on 0.0.0.0:" <> show port
      Warp.runSettings settings (app tenants)
