{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Monocle.Api.Config (Tenant (..), loadConfig) where

import Data.Aeson
import Data.Yaml as YAML
import Relude

newtype Tenant = Tenant {unTenant :: Text} deriving (Eq, Show)

instance FromJSON Tenant where
  parseJSON (Object v) = Tenant <$> v .: "index"
  parseJSON _ = mzero

newtype Tenants = Tenants {unTenants :: [Tenant]} deriving (Eq, Show)

instance FromJSON Tenants where
  parseJSON (Object v) = Tenants <$> v .: "tenants"
  parseJSON _ = mzero

loadConfig :: MonadIO m => FilePath -> m [Tenant]
loadConfig file = unTenants <$> YAML.decodeFileThrow file
