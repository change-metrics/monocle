{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The Monocle API HTTP type and servant
module Monocle.Api.HTTP (MonocleAPI, server) where

import Data.Aeson (ToJSON)
import qualified Monocle.Api.Config as Config
import Relude
import Servant

type MonocleAPI =
  "indices" :> Get '[JSON] [Text]
    :<|> "infos" :> Get '[JSON] Info

newtype Info = Info
  { version :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Info

info :: Info
info = Info "1.0.0"

server :: [Config.Tenant] -> Server MonocleAPI
server tenants =
  return (map Config.unTenant tenants)
    :<|> return info
