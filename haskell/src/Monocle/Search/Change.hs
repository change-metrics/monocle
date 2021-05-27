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

data File = File
  { fileAdditions :: Int,
    fileDeletions :: Int,
    filePath :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON File where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON File where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Commit = Commit
  { commitSha :: Text,
    commitAuthor :: Author,
    commitCommitter :: Author,
    commitAuthoredAt :: UTCTime,
    commitCommittedAt :: UTCTime,
    commitAdditions :: Int,
    commitDeletions :: Int,
    commitTitle :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Commit where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TaskData = TaskData
  { tdTid :: Text,
    tdTtype :: Text,
    tdUpdatedAt :: UTCTime,
    tdChangeUrl :: Text,
    tdSeverity :: Text,
    tdPriority :: Text,
    tdScore :: Int,
    tdUrl :: Text,
    tdTitle :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TaskData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON TaskData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Change = Change
  { changeNumber :: Int,
    changeChangeId :: Text,
    changeTitle :: Text,
    changeText :: Text,
    changeUrl :: Text,
    changeCommitCount :: Int,
    changeAdditions :: Int,
    changeDeletions :: Int,
    changeChangedFilesCount :: Int,
    changeChangedFiles :: [File],
    changeCommits :: [Commit],
    changeRepositoryPrefix :: Text,
    changeRepositoryShortname :: Text,
    changeRepositoryFullname :: Text,
    changeAuthor :: Author,
    changeCommitter :: Maybe Author,
    changeMergedBy :: Maybe Author,
    changeBranch :: Text,
    changeCreatedAt :: UTCTime,
    changeMergedAt :: Maybe UTCTime,
    changeUpdatedAt :: UTCTime,
    changeClosedAt :: Maybe UTCTime,
    changeState :: Text,
    changeDuration :: Maybe Int,
    changeMergeable :: Text,
    changeLabels :: [Text],
    changeAssignees :: [Author],
    changeApproval :: Maybe [Text],
    changeDraft :: Bool,
    changeSelfMerged :: Maybe Bool,
    changeTasksData :: Maybe [TaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Change where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Change where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
