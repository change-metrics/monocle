-- | Data types for ELK documents
module Monocle.Backend.Documents where

import Data.Aeson (FromJSON, ToJSON, Value (String), defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON, withText)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
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

-- | A custom utctime that supports optional 'Z' trailing suffix
newtype UTCTimePlus = UTCTimePlus UTCTime deriving stock (Show, Eq)

instance ToJSON UTCTimePlus where
  toJSON (UTCTimePlus utcTime) = String . toText . formatTime defaultTimeLocale "%FT%TZ" $ utcTime

instance FromJSON UTCTimePlus where
  parseJSON = withText "UTCTimePlus" (parse . toString)
    where
      oldFormat = "%FT%T"
      utcFormat = "%FT%TZ"
      tryParse f s = parseTimeM False defaultTimeLocale f s
      parse s = UTCTimePlus <$> (tryParse oldFormat s <|> tryParse utcFormat s)

data ETaskData = ETaskData
  { tdTid :: Text,
    tdTtype :: [Text],
    tdUpdatedAt :: UTCTimePlus,
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
      "ElkChangeState"
      ( \case
          "OPEN" -> pure EChangeOpen
          "MERGED" -> pure EChangeMerged
          "CLOSED" -> pure EChangeClosed
          _anyOtherValue -> fail "Unknown Monocle ELK change state"
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
      "ElkDocType"
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

data ELKChange = ELKChange
  { elkchangeId :: LText,
    elkchangeNumber :: Int,
    elkchangeType :: EDocType,
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
    elkchangeState :: EChangeState,
    elkchangeDuration :: Maybe Int,
    elkchangeMergeable :: LText,
    elkchangeLabels :: [LText],
    elkchangeAssignees :: [Author],
    elkchangeApproval :: Maybe [LText],
    elkchangeDraft :: Bool,
    elkchangeSelfMerged :: Maybe Bool,
    elkchangeTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKChange where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKChange where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype ELKChangeTD = ELKChangeTD
  { elkchangetdTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKChangeTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKChangeTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ELKChangeOrphanTD = ELKChangeOrphanTD
  { elkchangeorphantdId :: Text,
    elkchangeorphantdType :: EDocType,
    elkchangeorphantdTasksData :: ETaskData
  }
  deriving (Show, Eq, Generic)

instance ToJSON ELKChangeOrphanTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ELKChangeOrphanTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ELKTaskDataAdopted = ELKTaskDataAdopted
  {_adopted :: Text}
  deriving (Generic)

instance ToJSON ELKTaskDataAdopted where
  toJSON = genericToJSON defaultOptions

data ELKChangeOrphanTDAdopted = ELKChangeOrphanTDAdopted
  { elkchangeorphantdadptId :: Text,
    elkchangeorphantdadptType :: EDocType,
    elkchangeorphantdadptTasksData :: ELKTaskDataAdopted
  }
  deriving (Generic)

instance ToJSON ELKChangeOrphanTDAdopted where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data ELKChangeEvent = ELKChangeEvent
  { elkchangeeventId :: LText,
    elkchangeeventNumber :: Word32,
    elkchangeeventType :: EDocType,
    elkchangeeventChangeId :: LText,
    elkchangeeventUrl :: LText,
    elkchangeeventChangedFiles :: [SimpleFile],
    elkchangeeventRepositoryPrefix :: LText,
    elkchangeeventRepositoryShortname :: LText,
    elkchangeeventRepositoryFullname :: LText,
    -- elkchangeeventAuthor is optional due to the fact Gerrit closer
    -- does not set any author for ChangeAbandonedEvent
    elkchangeeventAuthor :: Maybe Author,
    elkchangeeventOnAuthor :: Author,
    elkchangeeventBranch :: LText,
    elkchangeeventOnCreatedAt :: UTCTime,
    elkchangeeventCreatedAt :: UTCTime,
    elkchangeeventApproval :: Maybe [LText],
    elkchangeeventTasksData :: Maybe [ETaskData]
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
