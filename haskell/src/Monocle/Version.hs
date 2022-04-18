{-# LANGUAGE TemplateHaskell #-}

-- | Monocle version
module Monocle.Version (version) where

import Data.Version (showVersion)
import Development.GitRev (gitDirty, gitHash)
import Language.Haskell.TH.Env (envQ)
import Paths_monocle qualified as P (version)
import Relude

version :: String
version = concat [showVersion P.version, " ", commit]
  where
    commit = case fromEnv of
      Just v -> v
      Nothing -> $(gitHash) <> dirty
    fromEnv :: Maybe String
    fromEnv = $$(envQ "MONOCLE_COMMIT")
    dirty = if $(gitDirty) then "-dirty" else ""
