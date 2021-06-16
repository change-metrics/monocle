{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Monocle.Api.Config
  ( Index (..),
    Crawler (..),
    Project (..),
    TaskCrawler (..),
    loadConfig,
    lookupTenant,
    lookupProject,
    lookupIdent,
    lookupGroupMembers,
    pname,
  )
where

import Data.Yaml as YAML
import qualified Dhall.TH
import Relude

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  [ Dhall.TH.SingleConstructor "Project" "Project" "./dhall-monocle/Monocle/Project/Type.dhall",
    Dhall.TH.SingleConstructor
      "Gerrit"
      "Gerrit"
      "./dhall-monocle/Monocle/Gerrit/Type.dhall",
    Dhall.TH.SingleConstructor
      "GitHub"
      "GitHub"
      "./dhall-monocle/Monocle/GitHub/Type.dhall",
    Dhall.TH.SingleConstructor
      "Crawler"
      "Crawler"
      "./dhall-monocle/Monocle/Crawler/Type.dhall",
    Dhall.TH.SingleConstructor
      "TaskCrawler"
      "TaskCrawler"
      "./dhall-monocle/Monocle/TaskCrawler/Type.dhall",
    Dhall.TH.SingleConstructor
      "Index"
      "Index"
      "./dhall-monocle/Monocle/Index/Type.dhall"
  ]

deriving instance FromJSON Gerrit

deriving instance Eq Gerrit

deriving instance Show Gerrit

deriving instance FromJSON GitHub

deriving instance Eq GitHub

deriving instance Show GitHub

deriving instance FromJSON Project

deriving instance Eq Project

deriving instance Show Project

deriving instance FromJSON TaskCrawler

deriving instance Eq TaskCrawler

deriving instance Show TaskCrawler

deriving instance FromJSON Crawler

deriving instance Eq Crawler

deriving instance Show Crawler

deriving instance FromJSON Index

deriving instance Eq Index

deriving instance Show Index

newtype Tenants = Tenants {unTenants :: [Index]} deriving (Eq, Show)

instance FromJSON Tenants where
  parseJSON (Object v) = Tenants <$> v .: "tenants"
  parseJSON _ = mzero

-- | Disambiguate the project name accessor
pname :: Project -> Text
pname = name

loadConfig :: MonadIO m => FilePath -> m [Index]
loadConfig fp = unTenants <$> YAML.decodeFileThrow fp

lookupTenant :: [Index] -> Text -> Maybe Index
lookupTenant xs tenantName = find isTenant xs
  where
    isTenant Index {..} = index == tenantName

lookupProject :: Index -> Text -> Maybe Project
lookupProject Index {..} projectName = find isProject (fromMaybe [] projects)
  where
    isProject :: Project -> Bool
    isProject Project {..} = name == projectName

-- | Need https://github.com/change-metrics/dhall-monocle/pull/3
lookupIdent :: Index -> Text -> Maybe Text
lookupIdent Index {..} userName = mempty

lookupGroupMembers :: Index -> Text -> Maybe (NonEmpty Text)
lookupGroupMembers Index {..} groupName = mempty
