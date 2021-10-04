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
