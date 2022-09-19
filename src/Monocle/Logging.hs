{-# LANGUAGE DeriveAnyClass #-}

-- | Monocle log events
module Monocle.Logging where

import Monocle.Entity
import Monocle.Prelude

data LogCrawlerContext = LogCrawlerContext
  { lccIndex :: Text
  , lccName :: Text
  , lccEntity :: Maybe Entity
  }
  deriving (Generic, ToJSON)

noContext :: LogCrawlerContext
noContext = LogCrawlerContext "<direct>" "CLI" Nothing
