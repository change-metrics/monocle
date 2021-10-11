-- | Data types for Elasticsearch documents
module Monocle.Backend.Documents where

import Data.Aeson (Value (String), defaultOptions, genericParseJSON, genericToJSON, withText)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Monocle.Prelude

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
  { commitSha :: LText,
    commitAuthor :: Author,
    commitCommitter :: Author,
    commitAuthoredAt :: UTCTime,
    commitCommittedAt :: UTCTime,
    commitAdditions :: Word32,
    commitDeletions :: Word32,
    commitTitle :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Commit where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ETaskData = ETaskData
  { tdTid :: Text,
    tdCrawlerName :: Text,
    tdTtype :: [Text],
    tdUpdatedAt :: MonocleTime,
    tdChangeUrl :: Text,
    tdSeverity :: Text,
    tdPriority :: Text,
    tdScore :: Int,
    tdUrl :: Text,
    tdTitle :: Text,
    tdPrefix :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ETaskData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ETaskData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data EChangeState
  = EChangeOpen
  | EChangeMerged
  | EChangeClosed
  deriving (Eq, Show)

changeStateToText :: EChangeState -> Text
changeStateToText = \case
  EChangeOpen -> "OPEN"
  EChangeMerged -> "MERGED"
  EChangeClosed -> "CLOSED"

instance ToJSON EChangeState where
  toJSON v = String $ toText $ changeStateToText v

instance FromJSON EChangeState where
  parseJSON =
    withText
      "EChangeState"
      ( \case
          "OPEN" -> pure EChangeOpen
          "MERGED" -> pure EChangeMerged
          "CLOSED" -> pure EChangeClosed
          _anyOtherValue -> fail "Unknown Monocle Elastic change state"
      )

data EDocType
  = EChangeCreatedEvent
  | EChangeMergedEvent
  | EChangeReviewedEvent
  | EChangeCommentedEvent
  | EChangeAbandonedEvent
  | EChangeCommitForcePushedEvent
  | EChangeCommitPushedEvent
  | EChangeDoc
  | EOrphanTaskData
  deriving (Eq, Show, Enum, Bounded)

allEventTypes :: [EDocType]
allEventTypes = filter (/= EChangeDoc) [minBound .. maxBound]

docTypeToText :: EDocType -> LText
docTypeToText = \case
  EChangeCreatedEvent -> "ChangeCreatedEvent"
  EChangeMergedEvent -> "ChangeMergedEvent"
  EChangeReviewedEvent -> "ChangeReviewedEvent"
  EChangeCommentedEvent -> "ChangeCommentedEvent"
  EChangeAbandonedEvent -> "ChangeAbandonedEvent"
  EChangeCommitForcePushedEvent -> "ChangeCommitForcePushedEvent"
  EChangeCommitPushedEvent -> "ChangeCommitPushedEvent"
  EChangeDoc -> "Change"
  EOrphanTaskData -> "OrphanTaskData"

eventTypesAsText :: [Text]
eventTypesAsText =
  toText . docTypeToText
    <$> [ EChangeCreatedEvent,
          EChangeMergedEvent,
          EChangeReviewedEvent,
          EChangeCommentedEvent,
          EChangeAbandonedEvent,
          EChangeCommitPushedEvent,
          EChangeCommitForcePushedEvent
        ]

instance ToJSON EDocType where
  toJSON v = String $ toText $ docTypeToText v

instance FromJSON EDocType where
  parseJSON =
    withText
      "EDocType"
      ( \case
          "ChangeCreatedEvent" -> pure EChangeCreatedEvent
          "ChangeMergedEvent" -> pure EChangeMergedEvent
          "ChangeReviewedEvent" -> pure EChangeReviewedEvent
          "ChangeCommentedEvent" -> pure EChangeCommentedEvent
          "ChangeAbandonedEvent" -> pure EChangeAbandonedEvent
          "ChangeCommitForcePushedEvent" -> pure EChangeCommitForcePushedEvent
          "ChangeCommitPushedEvent" -> pure EChangeCommitPushedEvent
          "Change" -> pure EChangeDoc
          "OrphanTaskData" -> pure EOrphanTaskData
          anyOtherValue -> fail $ "Unknown Monocle Elastic doc type: " <> toString anyOtherValue
      )

data EChange = EChange
  { echangeId :: LText,
    echangeNumber :: Int,
    echangeType :: EDocType,
    echangeChangeId :: LText,
    echangeTitle :: LText,
    echangeText :: LText,
    echangeUrl :: LText,
    echangeCommitCount :: Word32,
    echangeAdditions :: Word32,
    echangeDeletions :: Word32,
    echangeChangedFilesCount :: Word32,
    echangeChangedFiles :: [File],
    echangeCommits :: [Commit],
    echangeRepositoryPrefix :: LText,
    echangeRepositoryShortname :: LText,
    echangeRepositoryFullname :: LText,
    echangeAuthor :: Author,
    echangeMergedBy :: Maybe Author,
    echangeBranch :: LText,
    echangeTargetBranch :: LText,
    echangeCreatedAt :: UTCTime,
    echangeMergedAt :: Maybe UTCTime,
    echangeUpdatedAt :: UTCTime,
    echangeClosedAt :: Maybe UTCTime,
    echangeState :: EChangeState,
    echangeDuration :: Maybe Int,
    echangeMergeable :: LText,
    echangeLabels :: [LText],
    echangeAssignees :: [Author],
    echangeApproval :: Maybe [LText],
    echangeDraft :: Bool,
    echangeSelfMerged :: Maybe Bool,
    echangeTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChange where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChange where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype EChangeTD = EChangeTD
  { echangetdTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data EChangeOrphanTD = EChangeOrphanTD
  { echangeorphantdId :: Text,
    echangeorphantdType :: EDocType,
    echangeorphantdTasksData :: ETaskData
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeOrphanTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeOrphanTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ETaskDataAdopted = ETaskDataAdopted
  {_adopted :: Text}
  deriving (Generic)

instance ToJSON ETaskDataAdopted where
  toJSON = genericToJSON defaultOptions

data EChangeOrphanTDAdopted = EChangeOrphanTDAdopted
  { echangeorphantdadptId :: Text,
    echangeorphantdadptType :: EDocType,
    echangeorphantdadptTasksData :: ETaskDataAdopted
  }
  deriving (Generic)

instance ToJSON EChangeOrphanTDAdopted where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data EChangeEvent = EChangeEvent
  { echangeeventId :: LText,
    echangeeventNumber :: Word32,
    echangeeventType :: EDocType,
    echangeeventChangeId :: LText,
    echangeeventUrl :: LText,
    echangeeventChangedFiles :: [SimpleFile],
    echangeeventRepositoryPrefix :: LText,
    echangeeventRepositoryShortname :: LText,
    echangeeventRepositoryFullname :: LText,
    -- echangeeventAuthor is optional due to the fact Gerrit closer
    -- does not set any author for ChangeAbandonedEvent
    echangeeventAuthor :: Maybe Author,
    echangeeventOnAuthor :: Author,
    echangeeventBranch :: LText,
    echangeeventOnCreatedAt :: UTCTime,
    echangeeventCreatedAt :: UTCTime,
    echangeeventApproval :: Maybe [LText],
    echangeeventTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeEvent where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeEvent where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ECrawlerMetadataObject = ECrawlerMetadataObject
  { ecmCrawlerName :: LText,
    ecmCrawlerType :: LText,
    ecmCrawlerTypeValue :: LText,
    ecmLastCommitAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON ECrawlerMetadataObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ECrawlerMetadataObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype ECrawlerMetadata = ECrawlerMetadata
  { ecmCrawlerMetadata :: ECrawlerMetadataObject
  }
  deriving (Show, Eq, Generic)

instance ToJSON ECrawlerMetadata where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ECrawlerMetadata where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
