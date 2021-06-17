{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Monocle.Api.Config where

import Data.Yaml as YAML
import qualified Dhall.TH
import Relude

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  ( let providerPath name = "./dhall-monocle/Monocle/Provider/" <> name <> "/Type.dhall"
        provider name = Dhall.TH.SingleConstructor name name $ providerPath name
        mainPath name = "./dhall-monocle/Monocle/" <> name <> "/Type.dhall"
        main name = Dhall.TH.SingleConstructor name name $ mainPath name
     in [ main "Project",
          main "Ident",
          provider "Gerrit",
          provider "Gitlab",
          provider "Github",
          provider "Bugzilla",
          Dhall.TH.MultipleConstructors
            "Provider"
            "./dhall-monocle/Monocle/Lentille/Provider.dhall",
          -- To support backward compatible schema, we replace Index and Crawler schemas
          Dhall.TH.SingleConstructor "Index" "Index" $ mainPath "Tenant",
          Dhall.TH.SingleConstructor "Crawler" "Crawler" $ mainPath "Lentille"
        ]
  )

deriving instance FromJSON Gerrit

deriving instance Eq Gerrit

deriving instance Show Gerrit

deriving instance FromJSON Github

deriving instance Eq Github

deriving instance Show Github

deriving instance FromJSON Gitlab

deriving instance Eq Gitlab

deriving instance Show Gitlab

deriving instance FromJSON Bugzilla

deriving instance Eq Bugzilla

deriving instance Show Bugzilla

deriving instance FromJSON Project

deriving instance Eq Project

deriving instance Show Project

deriving instance FromJSON Provider

deriving instance Eq Provider

deriving instance Show Provider

deriving instance FromJSON Crawler

deriving instance Eq Crawler

deriving instance Show Crawler

deriving instance FromJSON Ident

deriving instance Eq Ident

deriving instance Show Ident

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
loadConfig fp =
  -- TODO: drop 'users', 'task_crawlers' and 'crawler' key
  unTenants <$> YAML.decodeFileThrow fp

lookupTenant :: [Index] -> Text -> Maybe Index
lookupTenant xs tenantName = find isTenant xs
  where
    isTenant Index {..} = index == tenantName

lookupProject :: Index -> Text -> Maybe Project
lookupProject Index {..} projectName = find isProject (fromMaybe [] projects)
  where
    isProject :: Project -> Bool
    isProject Project {..} = name == projectName

lookupCrawler :: Index -> Text -> Maybe Crawler
lookupCrawler Index {..} crawlerName = find isProject (fromMaybe [] crawlers)
  where
    isProject Crawler {..} = name == crawlerName

lookupIdent :: Index -> Text -> Maybe Text
lookupIdent Index {..} userName = getName <$> find isUser (fromMaybe [] idents)
  where
    getName Ident {..} = ident
    isUser Ident {..} = ident == userName

lookupGroupMembers :: Index -> Text -> Maybe (NonEmpty Text)
lookupGroupMembers Index {..} groupName = case foldr go [] (fromMaybe [] idents) of
  [] -> Nothing
  (x : xs) -> Just (x :| xs)
  where
    -- For each ident, check if it is a member of groupName.
    -- If it is, then add the ident name to the list
    go :: Ident -> [Text] -> [Text]
    go Ident {..} acc = case groups of
      Just xs
        | groupName `elem` xs -> ident : acc
        | otherwise -> acc
      Nothing -> acc

getCrawlerProject :: Crawler -> [Text]
getCrawlerProject Crawler {..} = case provider of
  GitlabProvider Gitlab {..} -> fromMaybe [] gitlab_repositories
  _ -> []
