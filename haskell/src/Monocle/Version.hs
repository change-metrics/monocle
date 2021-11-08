{-# LANGUAGE TemplateHaskell #-}

-- | Monocle version
module Monocle.Version (version) where

import Development.GitRev (gitDirty, gitHash)
import Relude

version :: String
version = $(gitHash) <> dirty
  where
    dirty
      | $(gitDirty) = "-dirty"
      | otherwise = ""
