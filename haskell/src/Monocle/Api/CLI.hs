{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.CLI (run) where

import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Monocle.Api.HTTP (MonocleAPI, server)
import qualified Monocle.Search.Queries as Q
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import Relude
import Servant (serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: [Config.Tenant] -> BH.BHEnv -> Wai.Application
app tenants bhEnv = serve monocleAPI (server tenants bhEnv)

run :: MonadIO m => Int -> Text -> FilePath -> m ()
run port elkUrl configFile = do
  tenants <- Config.loadConfig configFile
  bhEnv <- Q.mkEnv elkUrl
  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      putTextLn $
        "Serving " <> show (length tenants) <> " tenant(s) on 0.0.0.0:" <> show port <> " with elk: " <> elkUrl
      Warp.runSettings settings (app tenants bhEnv)
