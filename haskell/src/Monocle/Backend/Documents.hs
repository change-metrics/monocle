-- |
module Monocle.Backend.Documents where

import Data.Aeson (FromJSON, ToJSON, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time.Clock (UTCTime)
import Relude

data Author = Author
  { authorMuid :: LText,
    authorUid :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON Author where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Author where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data File = File
  { fileAdditions :: Word32,
    fileDeletions :: Word32,
    filePath :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON File where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON File where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype SimpleFile = SimpleFile
  { simplefilePath :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON SimpleFile where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SimpleFile where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Commit = Commit
  { elkcommitSha :: LText,
    elkcommitAuthor :: Author,
    elkcommitCommitter :: Author,
    elkcommitAuthoredAt :: UTCTime,
    elkcommitCommittedAt :: UTCTime,
    elkcommitAdditions :: Word32,
    elkcommitDeletions :: Word32,
    elkcommitTitle :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Commit where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- TODO: Replace by the existing Monocle.TaskData.NewTaskData
data TaskData = TaskData
  { tdTid :: Text,
    tdTtype :: [Text],
    -- TODO: Handle `2021-05-18T04:31:18` (without the trailing Z)
    -- tdUpdatedAt :: UTCTime,
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

data ELKChange = ELKChange
  { elkchangeId :: LText,
    elkchangeNumber :: Int,
    elkchangeType :: LText,
    elkchangeChangeId :: LText,
    elkchangeTitle :: LText,
    elkchangeText :: LText,
    elkchangeUrl :: LText,
    elkchangeCommitCount :: Word32,
    elkchangeAdditions :: Word32,
    elkchangeDeletions :: Word32,
    elkchangeChangedFilesCount :: Word32,
    elkchangeChangedFiles :: [File],
    elkchangeCommits :: [Commit],
    elkchangeRepositoryPrefix :: LText,
    elkchangeRepositoryShortname :: LText,
    elkchangeRepositoryFullname :: LText,
    elkchangeAuthor :: Author,
    elkchangeMergedBy :: Maybe Author,
    elkchangeBranch :: LText,
    elkchangeTargetBranch :: LText,
    elkchangeCreatedAt :: UTCTime,
    elkchangeMergedAt :: Maybe UTCTime,
    elkchangeUpdatedAt :: UTCTime,
    elkchangeClosedAt :: Maybe UTCTime,
    elkchangeState :: LText,
    elkchangeDuration :: Maybe Int,
    elkchangeMergeable :: LText,
    elkchangeLabels :: [LText],
    elkchangeAssignees :: [Author],
    elkchangeApproval :: Maybe [LText],
    elkchangeDraft :: Bool,
    elkchangeSelfMerged :: Maybe Bool,
    elkchangeTasksData :: Maybe [TaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKChange where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKChange where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ELKChangeEvent = ELKChangeEvent
  { elkchangeeventId :: LText,
    elkchangeeventNumber :: Word32,
    elkchangeeventType :: LText,
    elkchangeeventChangeId :: LText,
    elkchangeeventUrl :: LText,
    elkchangeeventChangedFiles :: [SimpleFile],
    elkchangeeventRepositoryPrefix :: LText,
    elkchangeeventRepositoryShortname :: LText,
    elkchangeeventRepositoryFullname :: LText,
    elkchangeeventAuthor :: Author,
    elkchangeeventOnAuthor :: Author,
    elkchangeeventBranch :: LText,
    elkchangeeventOnCreatedAt :: UTCTime,
    elkchangeeventCreatedAt :: UTCTime,
    elkchangeeventApproval :: Maybe [LText]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKChangeEvent where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKChangeEvent where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ELKCrawlerMetadataObject = ELKCrawlerMetadataObject
  { elkcmCrawlerName :: LText,
    elkcmCrawlerType :: LText,
    elkcmCrawlerTypeValue :: LText,
    elkcmLastCommitAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKCrawlerMetadataObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKCrawlerMetadataObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype ELKCrawlerMetadata = ELKCrawlerMetadata
  { elkcmCrawlerMetadata :: ELKCrawlerMetadataObject
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKCrawlerMetadata where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKCrawlerMetadata where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
