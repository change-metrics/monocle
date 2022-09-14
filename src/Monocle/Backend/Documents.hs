-- | Data types for Elasticsearch documents
--
-- Note there are three data types:
-- - Monocle.Change is the schemas used by the crawler, thus the import is named CrawlerPB to disambiguate.
-- - Monocle.Search is the schemas used by the web interface (SearchPB).
-- - Monocle.Backend.Documents (this module) is the schemas used by elastic search.
--
-- The different data types are used for different purpose:
-- - CrawlerPB are managed by Lentilles
-- - SearchPB is the public interface
-- - Documents needs to stay in sync with the elastic schemas.
--
-- This module provides From instance to help converting the data types.
module Monocle.Backend.Documents where

import Data.Aeson (Value (String), defaultOptions, genericParseJSON, genericToJSON, object, withObject, withText, (.:))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector qualified as V
import Monocle.Entity
import Monocle.Prelude
import Monocle.Protob.Change qualified as ChangePB
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Search qualified as SearchPB

data Author = Author
  { authorMuid :: LText
  , authorUid :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON Author where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Author where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance From ChangePB.Ident Author where
  from ChangePB.Ident {..} =
    Author
      { authorMuid = identMuid
      , authorUid = identUid
      }

-- | CachedAuthor is used by the Author search cache
data CachedAuthor = CachedAuthor
  { caType :: EDocType
  , caCachedAuthorMuid :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON CachedAuthor where
  toJSON c = genericToJSON (aesonPrefix snakeCase) $ c {caType = ECachedAuthor}

instance FromJSON CachedAuthor where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data File = File
  { fileAdditions :: Word32
  , fileDeletions :: Word32
  , filePath :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON File where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON File where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance From File SearchPB.File where
  from File {..} = SearchPB.File {..}

newtype SimpleFile = SimpleFile
  { simplefilePath :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON SimpleFile where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON SimpleFile where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Commit = Commit
  { commitSha :: LText
  , commitAuthor :: Author
  , commitCommitter :: Author
  , commitAuthoredAt :: UTCTime
  , commitCommittedAt :: UTCTime
  , commitAdditions :: Word32
  , commitDeletions :: Word32
  , commitTitle :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Commit where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | A helper function to ensure author value in PB message is defined
ensureAuthor :: Maybe ChangePB.Ident -> ChangePB.Ident
ensureAuthor = \case
  Just i -> i
  Nothing -> ChangePB.Ident "backend-ghost" "backend-host"

instance From ChangePB.Commit Commit where
  from ChangePB.Commit {..} =
    Commit
      { commitSha = commitSha
      , commitAuthor = from $ ensureAuthor commitAuthor
      , commitCommitter = from $ ensureAuthor commitCommitter
      , commitAuthoredAt = from $ fromMaybe (error "AuthoredAt field is mandatory") commitAuthoredAt
      , commitCommittedAt = from $ fromMaybe (error "CommittedAt field is mandatory") commitCommittedAt
      , commitDeletions = fromIntegral commitDeletions
      , commitAdditions = fromIntegral commitAdditions
      , commitTitle = commitTitle
      }

instance From Commit SearchPB.Commit where
  from Commit {..} =
    SearchPB.Commit
      { commitSha = commitSha
      , commitTitle = commitTitle
      , commitAuthor = authorMuid commitAuthor
      , commitAuthoredAt = Just $ from commitAuthoredAt
      , commitCommitter = authorMuid commitCommitter
      , commitCommittedAt = Just $ from commitCommittedAt
      , commitAdditions = commitAdditions
      , commitDeletions = commitDeletions
      }

-- | A custom utctime that supports optional 'Z' trailing suffix
newtype UTCTimePlus = UTCTimePlus UTCTime deriving stock (Show, Eq)

instance ToJSON UTCTimePlus where
  toJSON (UTCTimePlus utcTime) = String . from . formatTime defaultTimeLocale "%FT%TZ" $ utcTime

instance FromJSON UTCTimePlus where
  parseJSON = withText "UTCTimePlus" (parse . from)
   where
    oldFormat = "%FT%T"
    utcFormat = "%FT%TZ"
    tryParse = parseTimeM False defaultTimeLocale
    parse s = UTCTimePlus <$> (tryParse oldFormat s <|> tryParse utcFormat s)

data ETaskData = ETaskData
  { tdTid :: Text
  , tdCrawlerName :: Maybe Text
  , tdTtype :: [Text]
  , tdUpdatedAt :: MonocleTime
  , tdChangeUrl :: Text
  , tdSeverity :: Text
  , tdPriority :: Text
  , tdScore :: Int32
  , tdUrl :: Text
  , tdTitle :: Text
  , tdPrefix :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ETaskData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ETaskData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance From ETaskData SearchPB.TaskData where
  from td =
    let taskDataUpdatedAt = Nothing
        taskDataChangeUrl = from $ tdUrl td
        taskDataTtype = fmap from $ V.fromList $ tdTtype td
        taskDataTid = from $ tdTid td
        taskDataUrl = from $ tdUrl td
        taskDataTitle = from $ tdUrl td
        taskDataSeverity = from $ tdSeverity td
        taskDataPriority = from $ tdPriority td
        taskDataScore = from $ tdScore td
        taskDataPrefix = from $ fromMaybe "" $ tdPrefix td
     in SearchPB.TaskData {..}

data EChangeState
  = EChangeOpen
  | EChangeMerged
  | EChangeClosed
  deriving (Eq, Show)

instance From EChangeState Text where
  from = \case
    EChangeOpen -> "OPEN"
    EChangeMerged -> "MERGED"
    EChangeClosed -> "CLOSED"

instance From EChangeState LText where
  from = via @Text

instance ToJSON EChangeState where
  toJSON v = String $ from v

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
  | ECachedAuthor
  deriving (Eq, Show, Enum, Bounded)

allEventTypes :: [EDocType]
allEventTypes =
  filter (`notElem` [EChangeDoc, EOrphanTaskData, ECachedAuthor]) [minBound .. maxBound]

instance From EDocType Text where
  from = \case
    EChangeCreatedEvent -> "ChangeCreatedEvent"
    EChangeMergedEvent -> "ChangeMergedEvent"
    EChangeReviewedEvent -> "ChangeReviewedEvent"
    EChangeCommentedEvent -> "ChangeCommentedEvent"
    EChangeAbandonedEvent -> "ChangeAbandonedEvent"
    EChangeCommitForcePushedEvent -> "ChangeCommitForcePushedEvent"
    EChangeCommitPushedEvent -> "ChangeCommitPushedEvent"
    EChangeDoc -> "Change"
    EOrphanTaskData -> "OrphanTaskData"
    ECachedAuthor -> "CachedAuthor"

instance From EDocType LText where
  from = via @Text

eventTypesAsText :: [Text]
eventTypesAsText =
  from
    <$> [ EChangeCreatedEvent
        , EChangeMergedEvent
        , EChangeReviewedEvent
        , EChangeCommentedEvent
        , EChangeAbandonedEvent
        , EChangeCommitPushedEvent
        , EChangeCommitForcePushedEvent
        ]

instance ToJSON EDocType where
  toJSON v = String $ from v

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
          "CachedAuthor" -> pure ECachedAuthor
          anyOtherValue -> fail $ "Unknown Monocle Elastic doc type: " <> from anyOtherValue
      )

data EChange = EChange
  { echangeId :: LText
  , echangeNumber :: Int
  , echangeType :: EDocType
  , echangeChangeId :: LText
  , echangeTitle :: LText
  , echangeText :: LText
  , echangeUrl :: LText
  , echangeCommitCount :: Word32
  , echangeAdditions :: Word32
  , echangeDeletions :: Word32
  , echangeChangedFilesCount :: Word32
  , echangeChangedFiles :: [File]
  , echangeCommits :: [Commit]
  , echangeRepositoryPrefix :: LText
  , echangeRepositoryShortname :: LText
  , echangeRepositoryFullname :: LText
  , echangeAuthor :: Author
  , echangeMergedBy :: Maybe Author
  , echangeBranch :: LText
  , echangeTargetBranch :: LText
  , echangeCreatedAt :: UTCTime
  , echangeMergedAt :: Maybe UTCTime
  , echangeUpdatedAt :: UTCTime
  , echangeClosedAt :: Maybe UTCTime
  , echangeState :: EChangeState
  , echangeDuration :: Maybe Int
  , echangeMergeable :: LText
  , echangeLabels :: [LText]
  , echangeAssignees :: [Author]
  , echangeApproval :: Maybe [LText]
  , echangeDraft :: Bool
  , echangeSelfMerged :: Maybe Bool
  , echangeTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChange where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChange where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance From EChange SearchPB.Change where
  from change =
    let changeTitle = echangeTitle change
        changeUrl = echangeUrl change
        changeCreatedAt = (Just . from $ echangeCreatedAt change)
        changeUpdatedAt = (Just . from $ echangeUpdatedAt change)
        changeRepositoryFullname = echangeRepositoryFullname change
        changeState = from $ echangeState change
        changeBranch = echangeBranch change
        changeTargetBranch = echangeTargetBranch change
        changeTaskData = V.fromList . maybe [] (map from) $ echangeTasksData change
        changeChangeId = echangeChangeId change
        changeAuthor = authorMuid . echangeAuthor $ change
        changeText = echangeText change
        changeAdditions = echangeAdditions change
        changeDeletions = echangeDeletions change
        changeChangedFilesCount = echangeChangedFilesCount change
        changeApproval = V.fromList $ fromMaybe [] $ echangeApproval change
        changeAssignees = V.fromList (fmap authorMuid (echangeAssignees change))
        changeLabels = V.fromList $ echangeLabels change
        changeDraft = echangeDraft change
        changeMergeable = echangeMergeable change == "MERGEABLE"
        changeCommits = V.fromList . map from $ echangeCommits change
        changeChangedFiles = V.fromList . map from $ echangeChangedFiles change
        -- consistency rename from commit_count to commits_count
        changeCommitsCount = echangeCommitCount change
        changeMergedAt = Just . from =<< echangeMergedAt change
        changeMergedByM = Just . SearchPB.ChangeMergedByMMergedBy . authorMuid =<< echangeMergedBy change
        changeTtm = SearchPB.ChangeTtmDuration . fromInteger . toInteger <$> echangeDuration change
     in SearchPB.Change {..}

instance From ChangePB.Change EChange where
  from ChangePB.Change {..} =
    EChange
      { echangeId = changeId
      , echangeType = EChangeDoc
      , echangeTitle = changeTitle
      , echangeUrl = changeUrl
      , echangeCommitCount = fromInteger . toInteger $ changeCommitCount
      , echangeNumber = fromInteger . toInteger $ changeNumber
      , echangeChangeId = changeChangeId
      , echangeText = changeText
      , echangeAdditions = fromInteger $ toInteger changeAdditions
      , echangeDeletions = fromInteger $ toInteger changeDeletions
      , echangeChangedFilesCount = fromInteger $ toInteger changeChangedFilesCount
      , echangeChangedFiles = map from $ toList changeChangedFiles
      , echangeCommits = map from $ toList changeCommits
      , echangeRepositoryPrefix = changeRepositoryPrefix
      , echangeRepositoryFullname = changeRepositoryFullname
      , echangeRepositoryShortname = changeRepositoryShortname
      , echangeAuthor = from (ensureAuthor changeAuthor)
      , echangeMergedBy = toMergedByAuthor <$> changeOptionalMergedBy
      , echangeBranch = changeBranch
      , echangeTargetBranch = changeTargetBranch
      , echangeCreatedAt = from $ fromMaybe (error "CreatedAt field is mandatory") changeCreatedAt
      , echangeMergedAt = toMergedAt <$> changeOptionalMergedAt
      , echangeUpdatedAt = from $ fromMaybe (error "UpdatedAt field is mandatory") changeUpdatedAt
      , echangeClosedAt = toClosedAt <$> changeOptionalClosedAt
      , echangeState = toState $ fromPBEnum changeState
      , echangeDuration = toDuration <$> changeOptionalDuration
      , echangeMergeable = changeMergeable
      , echangeLabels = toList changeLabels
      , echangeAssignees = map (from . ensureAuthor) $ toList $ fmap Just changeAssignees
      , echangeApproval = Just $ toList changeApprovals
      , echangeDraft = changeDraft
      , echangeSelfMerged = toSelfMerged <$> changeOptionalSelfMerged
      , echangeTasksData = Nothing
      }
   where
    toMergedByAuthor (ChangePB.ChangeOptionalMergedByMergedBy m) = from $ ensureAuthor (Just m)
    toMergedAt (ChangePB.ChangeOptionalMergedAtMergedAt t) = from t
    toClosedAt (ChangePB.ChangeOptionalClosedAtClosedAt t) = from t
    toDuration (ChangePB.ChangeOptionalDurationDuration d) = fromInteger $ toInteger d
    toSelfMerged (ChangePB.ChangeOptionalSelfMergedSelfMerged b) = b
    toState cstate = case cstate of
      ChangePB.Change_ChangeStateOpen -> EChangeOpen
      ChangePB.Change_ChangeStateMerged -> EChangeMerged
      ChangePB.Change_ChangeStateClosed -> EChangeClosed

instance From ChangePB.ChangedFile File where
  from ChangePB.ChangedFile {..} =
    File (fromIntegral changedFileAdditions) (fromIntegral changedFileDeletions) changedFilePath

newtype EChangeTD = EChangeTD
  { echangetdTasksData :: Maybe [ETaskData]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data EChangeOrphanTD = EChangeOrphanTD
  { echangeorphantdId :: Text
  , echangeorphantdType :: EDocType
  , echangeorphantdTasksData :: ETaskData
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeOrphanTD where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeOrphanTD where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype ETaskDataAdopted = ETaskDataAdopted
  {_adopted :: Text}
  deriving (Generic)

instance ToJSON ETaskDataAdopted where
  toJSON = genericToJSON defaultOptions

data EChangeOrphanTDAdopted = EChangeOrphanTDAdopted
  { echangeorphantdadptId :: Text
  , echangeorphantdadptType :: EDocType
  , echangeorphantdadptTasksData :: ETaskDataAdopted
  }
  deriving (Generic)

instance ToJSON EChangeOrphanTDAdopted where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data EChangeEvent = EChangeEvent
  { echangeeventId :: LText
  , echangeeventNumber :: Word32
  , echangeeventType :: EDocType
  , echangeeventChangeId :: LText
  , echangeeventUrl :: LText
  , echangeeventChangedFiles :: [SimpleFile]
  , echangeeventRepositoryPrefix :: LText
  , echangeeventRepositoryShortname :: LText
  , echangeeventRepositoryFullname :: LText
  , -- echangeeventAuthor is optional due to the fact Gerrit closer
    -- does not set any author for ChangeAbandonedEvent
    echangeeventAuthor :: Maybe Author
  , echangeeventOnAuthor :: Author
  , echangeeventSelfMerged :: Maybe Bool
  , echangeeventBranch :: LText
  , -- Set labels as a Maybe type because existing indexes do not have the event docs with labels
    -- TODO: implement a migration procedure in the Janitor and remove the 'Maybe' from this value
    echangeeventLabels :: Maybe [LText]
  , echangeeventOnCreatedAt :: UTCTime
  , echangeeventCreatedAt :: UTCTime
  , echangeeventApproval :: Maybe [LText]
  , echangeeventTasksData :: Maybe [ETaskData]
  , echangeeventDuration :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeEvent where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeEvent where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance From EChangeEvent SearchPB.ChangeEvent where
  from EChangeEvent {..} =
    let changeEventId = echangeeventId
        changeEventType = from echangeeventType
        changeEventChangeId = echangeeventChangeId
        changeEventCreatedAt = Just . from $ echangeeventCreatedAt
        changeEventOnCreatedAt = Just . from $ echangeeventOnCreatedAt
        changeEventAuthor = maybe "backend-ghost" authorMuid echangeeventAuthor
        changeEventOnAuthor = authorMuid echangeeventOnAuthor
        changeEventBranch = echangeeventBranch
     in SearchPB.ChangeEvent {..}

data ECrawlerMetadataObject = ECrawlerMetadataObject
  { ecmCrawlerName :: Text
  , ecmCrawlerEntity :: Entity
  , ecmLastCommitAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- Note: [Crawler Metadata]
--
-- A better name for "crawler_type" and "crawler_type_value" is
-- "entity_type" and "entity_value". Though changing the names requires a data
-- migration procedure. Moreover, these names are not exposed,
-- only the matching Monocle.Entity value is available.
instance ToJSON ECrawlerMetadataObject where
  toJSON e =
    object
      [ ("crawler_name", toJSON (ecmCrawlerName e))
      , ("last_commit_at", toJSON (ecmLastCommitAt e))
      , ("crawler_type", String (entityTypeName (from $ ecmCrawlerEntity e)))
      , ("crawler_type_value", String entityValue)
      ]
   where
    -- WARNING: don't forget to update the FromJSON implementation below when changing the entity document encoding
    entityValue = case ecmCrawlerEntity e of
      Organization n -> n
      Project n -> n
      TaskDataEntity n -> n

instance FromJSON ECrawlerMetadataObject where
  parseJSON = withObject "CrawlerMetadataObject" $ \v -> do
    ecmCrawlerName <- v .: "crawler_name"
    ecmLastCommitAt <- v .: "last_commit_at"
    (etype :: Text) <- v .: "crawler_type"
    evalue <- v .: "crawler_type_value"
    ecmCrawlerEntity <- case etype of
      "organization" -> pure $ Organization evalue
      "project" -> pure $ Project evalue
      "taskdata" -> pure $ TaskDataEntity evalue
      _ -> fail $ "Unknown crawler entity type name: " <> from etype
    pure ECrawlerMetadataObject {..}

newtype ECrawlerMetadata = ECrawlerMetadata
  { ecmCrawlerMetadata :: ECrawlerMetadataObject
  }
  deriving (Show, Eq, Generic)

instance ToJSON ECrawlerMetadata where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ECrawlerMetadata where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- TODO: rename OldestEntity into crawler-metadata
instance From ECrawlerMetadataObject CrawlerPB.CommitInfoResponse_OldestEntity where
  from ecm = CrawlerPB.CommitInfoResponse_OldestEntity (Just (from $ ecmCrawlerEntity ecm)) (Just ts)
   where
    ts = from (ecmLastCommitAt ecm)
