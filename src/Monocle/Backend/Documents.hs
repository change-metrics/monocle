-- | Data types for Elasticsearch documents
--
-- Note [Documents type]
-- ~~~~~~~~~~~~~~~~~~~~~
--
-- To avoid tight coupling, the monocle data are represented in different type.
-- Checkout this article for some more explanation: https://leapyear.io/resources/blog-posts/how-to-haskell-sharing-data-types-is-tight-coupling/
--
-- There are three type of modules:
-- - Monocle.Protob.Change and Issue are created by the crawlers and read by the API.
-- - Monocle.Protob.Search are served by the API and read by the web interface.
-- - Monocle.Backend.Documents (this module) are managed by the API internal.
--
-- The different data types are used for different purpose:
-- - Monocle.Protob are the exchange format, the modules are imported using the 'PB' suffix.
-- - Documents (this module) are stored in elasticsearch, they must stay in sync with the index mapping.
--
-- Within an index, the different data type are sharing the same elasticsearch mapping.
-- It is the JSON decoder that decides what gets loaded based on the document type.
module Monocle.Backend.Documents where

import Data.Aeson (Value (String), defaultOptions, genericParseJSON, genericToJSON, withObject, withText, (.:))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types qualified
import Data.Text.Encoding.Base64 qualified as B64
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Vector qualified as V
import Monocle.Entity
import Monocle.Prelude
import Monocle.Protob.Change qualified as ChangePB
import Monocle.Protob.Crawler (CrawlerError (..))
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Issue qualified as IssuePB
import Monocle.Protob.Search qualified as SearchPB

data Author = Author
  { authorMuid :: LText
  , authorUid :: LText
  , authorGroups :: [LText]
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
      , authorGroups = toList identGroups
      }

fromMaybeIdent :: Maybe ChangePB.Ident -> Author
fromMaybeIdent = maybe ghostAuthor from
 where
  ghostAuthor = Author "backend-ghost" "backend-ghost" mempty

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
  Nothing -> ChangePB.Ident "backend-ghost" "backend-host" mempty

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

newtype EErrorData = EErrorData
  { eeErrorData :: EError
  }
  deriving (Show, Eq, Generic)

instance ToJSON EErrorData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EErrorData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Helper type to store binary text in elasticsearch using b64 encoding
newtype BinaryText = BinaryText Text
  deriving newtype (Show, Eq)

instance ToJSON BinaryText where
  toJSON = String . B64.encodeBase64 . from

instance FromJSON BinaryText where
  parseJSON = withText "binary" $ \v -> case B64.decodeBase64 v of
    Right x -> pure (BinaryText x)
    Left e -> fail ("Binary text decode failed: " <> from e)

instance From BinaryText Text where
  from (BinaryText txt) = txt

instance From BinaryText LText where
  from = via @Text

instance From LText BinaryText where
  from = BinaryText . from

data EError = EError
  { erCrawlerName :: Text
  , erEntity :: Entity
  , erCreatedAt :: UTCTime
  , erMessage :: Text
  , erBody :: BinaryText
  }
  deriving (Show, Eq, Generic)

instance From EError CrawlerError where
  from eerror =
    CrawlerError
      { crawlerErrorBody = from eerror.erBody
      , crawlerErrorMessage = from eerror.erMessage
      , crawlerErrorCreatedAt = Just $ from eerror.erCreatedAt
      }

-- Custom encoder to manually serialize the entity type
-- This needs to match the "error_data" schema above
instance ToJSON EError where
  toJSON e =
    object
      [ ("crawler_name", toJSON e.erCrawlerName)
      , ("created_at", toJSON e.erCreatedAt)
      , ("entity_type", String (entityTypeName (from e.erEntity)))
      , ("entity_value", String $ entityValue e.erEntity)
      , ("message", String e.erMessage)
      , ("body", toJSON e.erBody)
      ]

instance FromJSON EError where
  parseJSON = withObject "EError" $ \v -> do
    erCrawlerName <- v .: "crawler_name"
    erCreatedAt <- v .: "created_at"
    evalue <- v .: "entity_value"
    etype <- v .: "entity_type"
    erEntity <- parseEntity evalue etype
    erMessage <- v .: "message"
    erBody <- v .: "body"
    pure EError {..}

-- | Helper to encode entity
-- WARNING: don't forget to update the parseEntity implementation below when changing the entity document encoding
entityValue :: Entity -> Text
entityValue = \case
  Organization n -> n
  Project n -> n
  ProjectIssue n -> n
  TaskDataEntity n -> n
  User n -> n

parseEntity :: Text -> Text -> Data.Aeson.Types.Parser Entity
parseEntity evalue = \case
  "organization" -> pure $ Organization evalue
  "project" -> pure $ Project evalue
  "taskdata" -> pure $ TaskDataEntity evalue
  "user" -> pure $ User evalue
  etype -> fail $ "Unknown crawler entity type name: " <> from etype

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

-- | When adding new document type, update the `instance FromJSON EDocType` too.
data EDocType
  = EChangeCreatedEvent
  | EChangeMergedEvent
  | EChangeReviewedEvent
  | EChangeCommentedEvent
  | EChangeAbandonedEvent
  | EChangeCommitForcePushedEvent
  | EChangeCommitPushedEvent
  | EChangeDoc
  | EIssueCreatedEvent
  | EIssueClosedEvent
  | EIssueCommentedEvent
  | EIssueDoc
  | EOrphanTaskData
  | ECachedAuthor
  | EErrorDoc
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
    EIssueCreatedEvent -> "IssueCreatedEvent"
    EIssueClosedEvent -> "IssueClosedEvent"
    EIssueCommentedEvent -> "IssueCommentedEvent"
    EIssueDoc -> "Issue"
    EOrphanTaskData -> "OrphanTaskData"
    ECachedAuthor -> "CachedAuthor"
    EErrorDoc -> "Error"

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
          "IssueCreatedEvent" -> pure EIssueCreatedEvent
          "IssueClosedEvent" -> pure EIssueClosedEvent
          "IssueCommentedEvent" -> pure EIssueCommentedEvent
          "Issue" -> pure EIssueDoc
          "OrphanTaskData" -> pure EOrphanTaskData
          "CachedAuthor" -> pure ECachedAuthor
          "Error" -> pure EErrorDoc
          anyOtherValue -> fail $ "Unknown Monocle Elastic doc type: " <> from anyOtherValue
      )

data EIssue = EIssue
  { eissueId :: LText
  , eissueNumber :: Int
  , eissueType :: EDocType
  , eissueTitle :: LText
  , eissueText :: LText
  , eissueUrl :: LText
  , eissueRepositoryPrefix :: LText
  , eissueRepositoryShortname :: LText
  , eissueRepositoryFullname :: LText
  , eissueAuthor :: Author
  , eissueCreatedAt :: UTCTime
  , eissueUpdatedAt :: UTCTime
  , eissueClosedAt :: Maybe UTCTime
  , eissueState :: LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON EIssue where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EIssue where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Convert data comming from the crawler, (see Note [Documents type])
instance From IssuePB.Issue EIssue where
  from IssuePB.Issue {..} =
    EIssue
      { eissueId = issueId
      , eissueNumber = fromInteger . toInteger $ issueNumber
      , eissueType = EIssueDoc
      , eissueTitle = issueTitle
      , eissueText = issueText
      , eissueUrl = issueUrl
      , eissueRepositoryPrefix = issueRepositoryPrefix
      , eissueRepositoryShortname = issueRepositoryShortname
      , eissueRepositoryFullname = issueRepositoryFullname
      , eissueAuthor = from (ensureAuthor issueAuthor)
      , eissueCreatedAt = from $ fromMaybe (error "CreatedAt field is mandatory") issueCreatedAt
      , eissueUpdatedAt = from $ fromMaybe (error "UpdatedAt field is mandatory") issueUpdatedAt
      , eissueClosedAt = toClosedAt <$> issueOptionalClosedAt
      , eissueState = issueState
      }
   where
    toClosedAt (IssuePB.IssueOptionalClosedAtClosedAt t) = from t

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
  , echangeMergedCommitSha :: Maybe LText
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
      , echangeMergedCommitSha = toMergedCommitSha <$> changeOptionalMergedCommitSha
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
    toMergedCommitSha (ChangePB.ChangeOptionalMergedCommitShaMergedCommitSha sha) = sha

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

data EIssueEvent = EIssueEvent
  { eissueeventId :: LText
  , eissueeventNumber :: Int
  , eissueeventType :: EDocType
  , eissueeventAuthor :: Author
  , eissueeventComment :: Maybe LText
  }
  deriving (Show, Eq, Generic)

instance ToJSON EIssueEvent where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EIssueEvent where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Convert data comming from the crawler, (see Note [Documents type])
instance From IssuePB.IssueEvent EIssueEvent where
  from IssuePB.IssueEvent {..} =
    EIssueEvent
      { eissueeventId = issueEventId
      , eissueeventNumber = fromIntegral issueEventNumber
      , eissueeventType = maybe (error "issueEventType field is mandatory") getType issueEventType
      , eissueeventAuthor = fromMaybeIdent issueEventAuthor
      , eissueeventComment = commentFromType
      }
   where
    getType = \case
      IssuePB.IssueEventTypeIssueCreated {} -> EIssueCreatedEvent
      IssuePB.IssueEventTypeIssueClosed {} -> EIssueClosedEvent
      IssuePB.IssueEventTypeIssueCommented {} -> EIssueCommentedEvent
    commentFromType = case issueEventType of
      Just (IssuePB.IssueEventTypeIssueCommented (IssuePB.IssueCommentedEvent v)) -> Just v
      _ -> Nothing

data EChangeEvent = EChangeEvent
  { echangeeventId :: LText
  , echangeeventNumber :: Word32
  -- ^ this is odd, that should be Int, like in EChange
  , echangeeventType :: EDocType
  -- ^ the eventType value can only be one of EChange*Event
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
  , echangeeventTargetBranch :: LText
  , -- Set labels as a Maybe type because existing indexes do not have the event docs with labels
    -- TODO: implement a migration procedure in the Janitor and remove the 'Maybe' from this value
    echangeeventLabels :: Maybe [LText]
  , echangeeventOnCreatedAt :: UTCTime
  , echangeeventCreatedAt :: UTCTime
  , echangeeventApproval :: Maybe [LText]
  , echangeeventTasksData :: Maybe [ETaskData]
  , echangeeventDuration :: Maybe Int
  , echangeeventDraft :: Maybe Bool
  , echangeeventMergedCommitSha :: Maybe LText
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
      , ("crawler_type_value", String $ entityValue e.ecmCrawlerEntity)
      ]

instance FromJSON ECrawlerMetadataObject where
  parseJSON = withObject "CrawlerMetadataObject" $ \v -> do
    ecmCrawlerName <- v .: "crawler_name"
    ecmLastCommitAt <- v .: "last_commit_at"
    etype <- v .: "crawler_type"
    evalue <- v .: "crawler_type_value"
    ecmCrawlerEntity <- parseEntity evalue etype
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
