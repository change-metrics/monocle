{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Search.Change where

import Data.Aeson (FromJSON, ToJSON, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time.Clock (UTCTime)
import Relude

data Author = Author
  {authorMuid :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Author where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Author where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Change = Change
  { changeState :: Text,
    changeUrl :: Text,
    changeCreatedAt :: UTCTime,
    changeUpdatedAt :: UTCTime,
    changeTitle :: Text,
    changeRepositoryFullname :: Text,
    changeAuthor :: Author
  }
  deriving (Show, Eq, Generic)

instance ToJSON Change where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Change where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
