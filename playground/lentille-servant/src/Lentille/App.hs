{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The lentille application
module Lentille.App (app) where

#ifdef USE_OPENAPI
import Lentille.OpenApi (api, server)
#else
import Lentille.Api (api, server)
#endif

import Data.Time (getCurrentTime)
import Network.Wai (Application)
import Relude
import Servant (serve)

app :: MonadIO m => m Application
app = do
  now <- liftIO getCurrentTime
  db <- newIORef now
  pure $ serve api (server db)
