{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Monocle.Api.Config.Generated
-- Description : Handle the Monocle configuration file
-- Copyright   : Monocle authors
-- License     : AGPL-3
--
-- The module contains defintion of data types according to the
-- Monocle dhall schemas found in the schemas/monocle/config directory. It also
-- provides some functions to handle configuration data.
module Monocle.Config.Generated (
  -- * Data types imported from dhall
  Project (..),
  Ident (..),
  SearchAlias (..),
  About (..),
  Crawlers (..),
  Crawler (..),
  Provider (..),
  Gitlab (..),
  Gerrit (..),
  Github (..),
  GithubUser (..),
  Bugzilla (..),
  GithubApplication (..),
  Link (..),
  Auth (..),
  AuthProvider (..),
  OIDC (..),
  GithubAuth (..),

  -- * Extra Generated
  configurationSchema,
) where

import Dhall.Core qualified
import Dhall.Src qualified
import Dhall.TH qualified
import Monocle.Prelude

-- | Generate Haskell Type from Dhall Type
-- See: https://hackage.haskell.org/package/dhall-1.38.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  ( let providerPath name = "./schemas/monocle/config/Provider/" <> name <> "/Type.dhall"
        authProviderPath name = "./schemas/monocle/config/AuthProvider/" <> name <> "/Type.dhall"
        provider name = Dhall.TH.SingleConstructor name name $ providerPath name
        authProvider name = Dhall.TH.SingleConstructor name name $ authProviderPath name
        mainPath name = "./schemas/monocle/config/" <> name <> "/Type.dhall"
        main name = Dhall.TH.SingleConstructor name name $ mainPath name
     in [ main "Project"
        , main "Ident"
        , main "SearchAlias"
        , main "Crawler"
        , main "Crawlers"
        , main "Auth"
        , main "About"
        , main "Link"
        , provider "Gerrit"
        , provider "Gitlab"
        , provider "Github"
        , provider "GithubUser"
        , provider "GithubApplication"
        , provider "Bugzilla"
        , authProvider "OIDC"
        , authProvider "GithubAuth"
        , Dhall.TH.MultipleConstructors
            "Provider"
            "./schemas/monocle/config/Crawler/Provider.dhall"
        , Dhall.TH.MultipleConstructors
            "AuthProvider"
            "./schemas/monocle/config/Auth/Provider.dhall"
        ]
  )

-- | Embed the expected configuration schema
configurationSchema :: Dhall.Core.Expr Dhall.Src.Src Void
configurationSchema = $(Dhall.TH.staticDhallExpression "./schemas/monocle/config/Config/Type.dhall")

deriving instance Eq OIDC
deriving instance Show OIDC

deriving instance Eq GithubAuth
deriving instance Show GithubAuth

deriving instance Eq Auth
deriving instance Show Auth

deriving instance Eq AuthProvider
deriving instance Show AuthProvider

deriving instance Eq Gerrit
deriving instance Show Gerrit

deriving instance Eq Github
deriving instance Show Github

deriving instance Eq GithubUser
deriving instance Show GithubUser

deriving instance Eq GithubApplication
deriving instance Show GithubApplication

deriving instance Eq Gitlab
deriving instance Show Gitlab

deriving instance Eq Bugzilla
deriving instance Show Bugzilla

deriving instance Eq Project
deriving instance Show Project

deriving instance Eq Provider
deriving instance Show Provider

deriving instance Eq Crawlers
deriving instance Show Crawlers

deriving instance Eq Crawler
deriving instance Show Crawler

deriving instance Eq Ident
deriving instance Show Ident

deriving instance Eq SearchAlias
deriving instance Show SearchAlias

deriving instance Eq Link
deriving instance Show Link

deriving instance Eq About
deriving instance Show About

-- End - Loading of Types from the dhall-monocle
