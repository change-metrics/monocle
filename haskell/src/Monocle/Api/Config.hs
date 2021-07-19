{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Monocle.Api.Config where

import qualified Data.ByteString as BS
import Data.Either.Validation (Validation (Failure, Success))
import qualified Data.Map as Map
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Src
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
          main "SearchAlias",
          main "Crawler",
          provider "Gerrit",
          provider "Gitlab",
          provider "Github",
          provider "Bugzilla",
          Dhall.TH.MultipleConstructors
            "Provider"
            "./dhall-monocle/Monocle/Crawler/Provider.dhall",
          -- To support backward compatible schema, we replace Index and Crawler schemas
          Dhall.TH.SingleConstructor "Index" "Index" $ mainPath "Tenant",
          Dhall.TH.SingleConstructor "Config" "Config" "./dhall-monocle/Monocle/Config.dhall"
        ]
  )

-- | Embed the expected configuration schema
configurationSchema :: Dhall.Core.Expr Dhall.Src.Src Void
configurationSchema = $(Dhall.TH.staticDhallExpression "./dhall-monocle/Monocle/Config.dhall")

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

deriving instance Eq SearchAlias

deriving instance Show SearchAlias

deriving instance Eq Index

deriving instance Show Index

-- | Disambiguate the project name accessor
pname :: Project -> Text
pname = name

-- | Load the YAML config file
loadConfig :: MonadIO m => FilePath -> m (Either Text [Index])
loadConfig fp = do
  -- Here we use the yaml-to-dhall logic to correctly decode Union value.
  -- Otherwise the decoder may fail with:
  -- AesonException "Error in $.tenants[1].crawlers[0].provider: parsing "
  --   Monocle.Api.Config.Provider(GitlabProvider) failed, key \"contents\" not found"
  --
  -- dhallFromYaml is able to infer the sum type by its value and it picks
  -- the first constructor that fit.
  expr <- liftIO $ Dhall.dhallFromYaml loadOpt =<< BS.readFile fp
  pure $ case Dhall.extract Dhall.auto expr of
    Success config -> Right $ tenants config
    Failure err -> Left $ show err
  where
    configType = Dhall.Core.pretty configurationSchema
    loadOpt = Dhall.defaultOptions $ Just configType

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
lookupCrawler Index {..} crawlerName = find isProject crawlers
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

getAliases :: Index -> [(Text, Text)]
getAliases index = maybe [] (fmap toTuple) (search_aliases index)
  where
    toTuple SearchAlias {..} = (name, alias)

getCrawlerProject :: Crawler -> [Text]
getCrawlerProject Crawler {..} = case provider of
  GitlabProvider Gitlab {..} -> fromMaybe [] gitlab_repositories
  otherProvider -> error $ "Provider not supported: " <> show otherProvider

getCrawlerOrganization :: Crawler -> [Text]
getCrawlerOrganization Crawler {..} = case provider of
  GitlabProvider Gitlab {..} -> fromMaybe [] gitlab_organizations
  otherProvider -> error $ "Provider not supported: " <> show otherProvider

emptyTenant :: Text -> [Ident] -> Index
emptyTenant name idents' =
  let crawlers_api_key = ""
      crawlers = []
      projects = Nothing
      idents = Just idents'
      search_aliases = Nothing
      index = name
   in Index {..}

createIdent :: Text -> [Text] -> [Text] -> Ident
createIdent name aliases' groups' =
  let ident = name
      aliases = aliases'
      groups = Just groups'
   in Ident {..}

-- | Get the list of group and members
--
-- >>> getTenantGroups (emptyTenant "test" [createIdent "alice" [] ["core", "ptl"], createIdent "bob" [] ["core"]])
-- [("core",["bob","alice"]),("ptl",["alice"])]
getTenantGroups :: Index -> [(Text, [Text])]
getTenantGroups Index {..} = Map.toList $ foldr go mempty (fromMaybe [] idents)
  where
    go :: Ident -> Map Text [Text] -> Map Text [Text]
    go Ident {..} acc = foldr (addUser ident) acc (fromMaybe [] groups)
    addUser :: Text -> Text -> Map Text [Text] -> Map Text [Text]
    addUser name groupName acc =
      let users' = fromMaybe [] (Map.lookup groupName acc)
       in Map.insert groupName (users' <> [name]) acc

-- | Get the Ident name for a Given alias
--
-- >>> :{
--  let
--    index = emptyTenant "test" [createIdent "alice" ["opendev.org/Alice Doe/12345", "github.com/alice89"] []]
--  in getIdentByAlias index "github.com/alice89"
-- :}
-- Just "alice"

-- >>> :{
--  let
--    index = emptyTenant "test" [createIdent "alice" ["opendev.org/Alice Doe/12345", "github.com/alice89"] []]
--  in getIdentByAlias index "github.com/ghost"
-- :}
-- Nothing
getIdentByAlias :: Index -> Text -> Maybe Text
getIdentByAlias Index {..} alias = getIdentByAliasFromIdents alias =<< idents

getIdentByAliasFromIdents :: Text -> [Ident] -> Maybe Text
getIdentByAliasFromIdents alias idents' = case isMatched <$> idents' of
  [] -> Nothing
  x : _ -> x
  where
    isMatched :: Ident -> Maybe Text
    isMatched Ident {..} = if alias `elem` aliases then Just ident else Nothing
