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

import qualified Data.ByteString as BS
import Data.Either.Validation (Validation (Failure, Success))
import Data.FileEmbed (embedFile)
import qualified Dhall
import qualified Dhall.TH
import qualified Dhall.YamlToDhall as Dhall
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
          Dhall.TH.SingleConstructor "Crawler" "Crawler" $ mainPath "Lentille",
          Dhall.TH.SingleConstructor "Config" "Config" "./dhall-monocle/Monocle/Config.dhall"
        ]
  )

deriving instance Eq Gerrit

deriving instance Show Gerrit

deriving instance Eq Github

deriving instance Show Github

deriving instance Eq Gitlab

deriving instance Show Gitlab

deriving instance Eq Bugzilla

deriving instance Show Bugzilla

deriving instance Eq Project

deriving instance Show Project

deriving instance Eq Provider

deriving instance Show Provider

deriving instance Eq Crawler

deriving instance Show Crawler

deriving instance Eq Ident

deriving instance Show Ident

deriving instance Eq Index

deriving instance Show Index

-- | Disambiguate the project name accessor
pname :: Project -> Text
pname = name

-- | Load the YAML config file
loadConfig :: MonadIO m => FilePath -> m [Index]
loadConfig fp = do
  -- Here we use the yaml-to-dhall logic to correctly decode Union value.
  -- Otherwise the decoder may fail with:
  -- AesonException "Error in $.tenants[1].crawlers[0].provider: parsing "
  --   Monocle.Api.Config.Provider(GitlabProvider) failed, key \"contents\" not found"
  --
  -- dhallFromYaml is able to infer the sum type by its value and it picks
  -- the first constructor that fit.
  expr <- liftIO $ Dhall.dhallFromYaml loadOpt =<< BS.readFile fp
  case Dhall.extract Dhall.auto expr of
    Success config -> pure (tenants config)
    Failure err -> error (show err)
  where
    -- when updating the dhall-monocle schema, the config needs to be
    -- regenerated using:
    -- `dhall <<< ./dhall-monocle/Monocle/Config.dhall > test/data/Config.dhall`
    configType = $(embedFile "./test/data/Config.dhall")
    loadOpt = Dhall.defaultOptions $ Just $ decodeUtf8 configType

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
