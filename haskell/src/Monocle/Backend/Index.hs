-- | Index management functions such as document mapping and ingest
module Monocle.Backend.Index where

import Data.Aeson
  ( KeyValue ((.=)),
    object,
  )
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashTable.IO as H
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Database.Bloodhound.Raw as BHR
import Google.Protobuf.Timestamp as T
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents
import Monocle.Change
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Env
import Monocle.Prelude
import Monocle.TaskData
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as NHTS

data ChangesIndexMapping = ChangesIndexMapping deriving (Eq, Show)

instance ToJSON ChangesIndexMapping where
  toJSON ChangesIndexMapping =
    object
      [ "properties"
          .= object
            [ "id" .= object ["type" .= ("keyword" :: Text)],
              "type" .= object ["type" .= ("keyword" :: Text)],
              "number" .= object ["type" .= ("keyword" :: Text)],
              "change_id" .= object ["type" .= ("keyword" :: Text)],
              "title"
                .= object
                  [ "type" .= ("text" :: Text),
                    "fields"
                      .= object
                        [ "keyword"
                            .= object
                              [ "type" .= ("keyword" :: Text),
                                "ignore_above" .= (8191 :: Int)
                              ]
                        ]
                  ],
              "text"
                .= object
                  [ "type" .= ("text" :: Text),
                    "fields"
                      .= object
                        [ "keyword"
                            .= object
                              [ "type" .= ("keyword" :: Text),
                                "ignore_above" .= (8191 :: Int)
                              ]
                        ]
                  ],
              "url" .= object ["type" .= ("keyword" :: Text)],
              "commit_count" .= object ["type" .= ("integer" :: Text)],
              "additions" .= object ["type" .= ("integer" :: Text)],
              "deletions" .= object ["type" .= ("integer" :: Text)],
              "change_files_count" .= object ["type" .= ("integer" :: Text)],
              "changed_files"
                .= object
                  [ "properties"
                      .= object
                        [ "additions" .= object ["type" .= ("integer" :: Text)],
                          "deletions" .= object ["type" .= ("integer" :: Text)],
                          "path" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "commits"
                .= object
                  [ "properties"
                      .= object
                        [ "sha" .= object ["type" .= ("keyword" :: Text)],
                          "author"
                            .= object
                              [ "properties"
                                  .= object
                                    [ "uid" .= object ["type" .= ("keyword" :: Text)],
                                      "muid" .= object ["type" .= ("keyword" :: Text)]
                                    ]
                              ],
                          "authored_at"
                            .= object
                              [ "type" .= ("date" :: Text),
                                "format" .= ("date_time_no_millis" :: Text)
                              ],
                          "committed_at"
                            .= object
                              [ "type" .= ("date" :: Text),
                                "format" .= ("date_time_no_millis" :: Text)
                              ],
                          "additions" .= object ["type" .= ("integer" :: Text)],
                          "deletions" .= object ["type" .= ("integer" :: Text)],
                          "title" .= object ["type" .= ("text" :: Text)]
                        ]
                  ],
              "repository_prefix" .= object ["type" .= ("keyword" :: Text)],
              "repository_fullname" .= object ["type" .= ("keyword" :: Text)],
              "repository_shortname" .= object ["type" .= ("keyword" :: Text)],
              "author"
                .= object
                  [ "properties"
                      .= object
                        [ "uid" .= object ["type" .= ("keyword" :: Text)],
                          "muid" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "on_author"
                .= object
                  [ "properties"
                      .= object
                        [ "uid" .= object ["type" .= ("keyword" :: Text)],
                          "muid" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "committer"
                .= object
                  [ "properties"
                      .= object
                        [ "uid" .= object ["type" .= ("keyword" :: Text)],
                          "muid" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "merged_by"
                .= object
                  [ "properties"
                      .= object
                        [ "uid" .= object ["type" .= ("keyword" :: Text)],
                          "muid" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "branch" .= object ["type" .= ("keyword" :: Text)],
              "target_branch" .= object ["type" .= ("keyword" :: Text)],
              "created_at"
                .= object
                  [ "type" .= ("date" :: Text),
                    "format" .= ("date_time_no_millis" :: Text)
                  ],
              "on_created_at"
                .= object
                  [ "type" .= ("date" :: Text),
                    "format" .= ("date_time_no_millis" :: Text)
                  ],
              "merged_at"
                .= object
                  [ "type" .= ("date" :: Text),
                    "format" .= ("date_time_no_millis" :: Text)
                  ],
              "updated_at"
                .= object
                  [ "type" .= ("date" :: Text),
                    "format" .= ("date_time_no_millis" :: Text)
                  ],
              "closed_at"
                .= object
                  [ "type" .= ("date" :: Text),
                    "format" .= ("date_time_no_millis" :: Text)
                  ],
              "state"
                .= object ["type" .= ("keyword" :: Text)],
              "duration" .= object ["type" .= ("integer" :: Text)],
              "mergeable" .= object ["type" .= ("keyword" :: Text)],
              "labels" .= object ["type" .= ("keyword" :: Text)],
              "assignees"
                .= object
                  [ "type" .= ("nested" :: Text),
                    "properties"
                      .= object
                        [ "uid" .= object ["type" .= ("keyword" :: Text)],
                          "muid" .= object ["type" .= ("keyword" :: Text)]
                        ]
                  ],
              "approval" .= object ["type" .= ("keyword" :: Text)],
              "draft" .= object ["type" .= ("boolean" :: Text)],
              "self_merged" .= object ["type" .= ("boolean" :: Text)],
              "crawler_metadata"
                .= object
                  [ "properties"
                      .= object
                        [ "crawler_name" .= object ["type" .= ("keyword" :: Text)],
                          "crawler_type" .= object ["type" .= ("keyword" :: Text)],
                          "crawler_type_value" .= object ["type" .= ("keyword" :: Text)],
                          "last_commit_at"
                            .= object
                              [ "type" .= ("date" :: Text),
                                "format" .= ("date_time_no_millis" :: Text)
                              ],
                          "last_post_at"
                            .= object
                              [ "type" .= ("date" :: Text),
                                "format" .= ("date_time_no_millis" :: Text)
                              ],
                          "total_docs_posted" .= object ["type" .= ("integer" :: Text)],
                          "total_changes_updated" .= object ["type" .= ("integer" :: Text)],
                          "total_change_events_updated" .= object ["type" .= ("integer" :: Text)],
                          "total_orphans_updated" .= object ["type" .= ("integer" :: Text)]
                        ]
                  ],
              "tasks_data"
                .= object
                  [ "properties"
                      .= object
                        [ "tid" .= object ["type" .= ("keyword" :: Text)],
                          "ttype" .= object ["type" .= ("keyword" :: Text)],
                          "crawler_name" .= object ["type" .= ("keyword" :: Text)],
                          "updated_at"
                            .= object
                              [ "type" .= ("date" :: Text),
                                "format" .= ("date_time_no_millis" :: Text)
                              ],
                          "change_url" .= object ["type" .= ("keyword" :: Text)],
                          "severity" .= object ["type" .= ("keyword" :: Text)],
                          "priority" .= object ["type" .= ("keyword" :: Text)],
                          "score" .= object ["type" .= ("integer" :: Text)],
                          "url" .= object ["type" .= ("keyword" :: Text)],
                          "prefix" .= object ["type" .= ("keyword" :: Text)],
                          "title"
                            .= object
                              [ "type" .= ("text" :: Text),
                                "fields"
                                  .= object
                                    [ "keyword"
                                        .= object
                                          [ "type" .= ("keyword" :: Text),
                                            "ignore_above" .= (8191 :: Int)
                                          ]
                                    ]
                              ],
                          "_adopted" .= object ["type" .= ("boolean" :: Text)]
                        ]
                  ]
            ]
      ]

ensureIndex :: TenantM ()
ensureIndex = do
  indexName <- getIndexName
  config <- getIndexConfig
  _respCI <- BH.createIndex indexSettings indexName
  BHR.settings indexName (object ["index" .= object ["max_regex_length" .= (50_000 :: Int)]])
  -- print respCI
  _respPM <- BH.putMapping indexName ChangesIndexMapping
  -- print respPM
  True <- BH.indexExists indexName
  traverse_ initCrawlerMetadata $ Config.crawlers config
  where
    indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0)

removeIndex :: TenantM ()
removeIndex = do
  indexName <- getIndexName
  _resp <- BH.deleteIndex indexName
  False <- BH.indexExists indexName
  pure ()

toAuthor :: Maybe Monocle.Change.Ident -> Monocle.Backend.Documents.Author
toAuthor (Just Monocle.Change.Ident {..}) =
  Monocle.Backend.Documents.Author
    { authorMuid = identMuid,
      authorUid = identUid
    }
toAuthor Nothing =
  Monocle.Backend.Documents.Author
    "backend-ghost"
    "backend-ghost"

toEChangeEvent :: ChangeEvent -> EChangeEvent
toEChangeEvent ChangeEvent {..} =
  EChangeEvent
    { echangeeventId = changeEventId,
      echangeeventNumber = fromIntegral changeEventNumber,
      echangeeventType = getEventType changeEventType,
      echangeeventChangeId = changeEventChangeId,
      echangeeventUrl = changeEventUrl,
      echangeeventChangedFiles = SimpleFile . changedFilePathPath <$> toList changeEventChangedFiles,
      echangeeventRepositoryPrefix = changeEventRepositoryPrefix,
      echangeeventRepositoryFullname = changeEventRepositoryFullname,
      echangeeventRepositoryShortname = changeEventRepositoryShortname,
      echangeeventAuthor = Just $ toAuthor changeEventAuthor,
      echangeeventOnAuthor = toAuthor changeEventOnAuthor,
      echangeeventBranch = changeEventBranch,
      echangeeventCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventCreatedAt field is mandatory") changeEventCreatedAt,
      echangeeventOnCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventOnCreatedAt field is mandatory") changeEventOnCreatedAt,
      echangeeventApproval = case changeEventType of
        Just (ChangeEventTypeChangeReviewed (ChangeReviewedEvent approval)) -> Just $ toList approval
        _anyOtherApprovals -> Nothing,
      echangeeventTasksData = Nothing
    }
  where
    getEventType :: Maybe ChangeEventType -> EDocType
    getEventType eventTypeM = case eventTypeM of
      Just eventType -> case eventType of
        ChangeEventTypeChangeCreated ChangeCreatedEvent -> EChangeCreatedEvent
        ChangeEventTypeChangeCommented ChangeCommentedEvent -> EChangeCommentedEvent
        ChangeEventTypeChangeAbandoned ChangeAbandonedEvent -> EChangeAbandonedEvent
        ChangeEventTypeChangeReviewed (ChangeReviewedEvent _) -> EChangeReviewedEvent
        ChangeEventTypeChangeCommitForcePushed ChangeCommitForcePushedEvent -> EChangeCommitForcePushedEvent
        ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent -> EChangeCommitPushedEvent
        ChangeEventTypeChangeMerged ChangeMergedEvent -> EChangeMergedEvent
      Nothing -> error "changeEventType field is mandatory"

toEChange :: Change -> EChange
toEChange Change {..} =
  EChange
    { echangeId = changeId,
      echangeType = EChangeDoc,
      echangeTitle = changeTitle,
      echangeUrl = changeUrl,
      echangeCommitCount = fromInteger . toInteger $ changeCommitCount,
      echangeNumber = fromInteger . toInteger $ changeNumber,
      echangeChangeId = changeChangeId,
      echangeText = changeText,
      echangeAdditions = fromInteger $ toInteger changeAdditions,
      echangeDeletions = fromInteger $ toInteger changeDeletions,
      echangeChangedFilesCount = fromInteger $ toInteger changeChangedFilesCount,
      echangeChangedFiles = map toFile $ toList changeChangedFiles,
      echangeCommits = map toCommit $ toList changeCommits,
      echangeRepositoryPrefix = changeRepositoryPrefix,
      echangeRepositoryFullname = changeRepositoryFullname,
      echangeRepositoryShortname = changeRepositoryShortname,
      echangeAuthor = toAuthor changeAuthor,
      echangeMergedBy = toMergedByAuthor <$> changeOptionalMergedBy,
      echangeBranch = changeBranch,
      echangeTargetBranch = changeTargetBranch,
      echangeCreatedAt = T.toUTCTime $ fromMaybe (error "CreatedAt field is mandatory") changeCreatedAt,
      echangeMergedAt = toMergedAt <$> changeOptionalMergedAt,
      echangeUpdatedAt = T.toUTCTime $ fromMaybe (error "UpdatedAt field is mandatory") changeUpdatedAt,
      echangeClosedAt = toClosedAt <$> changeOptionalClosedAt,
      echangeState = toState $ fromPBEnum changeState,
      echangeDuration = toDuration <$> changeOptionalDuration,
      echangeMergeable = changeMergeable,
      echangeLabels = toList changeLabels,
      echangeAssignees = map toAuthor $ toList $ fmap Just changeAssignees,
      echangeApproval = Just $ toList changeApprovals,
      echangeDraft = changeDraft,
      echangeSelfMerged = toSelfMerged <$> changeOptionalSelfMerged,
      echangeTasksData = Nothing
    }
  where
    toFile :: ChangedFile -> File
    toFile ChangedFile {..} =
      File (fromIntegral changedFileAdditions) (fromIntegral changedFileDeletions) changedFilePath
    toCommit :: Monocle.Change.Commit -> Monocle.Backend.Documents.Commit
    toCommit Monocle.Change.Commit {..} =
      Monocle.Backend.Documents.Commit
        { commitSha = commitSha,
          commitAuthor = toAuthor commitAuthor,
          commitCommitter = toAuthor commitCommitter,
          commitAuthoredAt = T.toUTCTime $ fromMaybe (error "AuthoredAt field is mandatory") commitAuthoredAt,
          commitCommittedAt = T.toUTCTime $ fromMaybe (error "CommittedAt field is mandatory") commitCommittedAt,
          commitDeletions = fromIntegral commitDeletions,
          commitAdditions = fromIntegral commitAdditions,
          commitTitle = commitTitle
        }
    toMergedByAuthor (ChangeOptionalMergedByMergedBy m) = toAuthor (Just m)
    toMergedAt (ChangeOptionalMergedAtMergedAt t) = T.toUTCTime t
    toClosedAt (ChangeOptionalClosedAtClosedAt t) = T.toUTCTime t
    toDuration (ChangeOptionalDurationDuration d) = fromInteger $ toInteger d
    toSelfMerged (ChangeOptionalSelfMergedSelfMerged b) = b
    toState cstate = case cstate of
      Change_ChangeStateOpen -> EChangeOpen
      Change_ChangeStateMerged -> EChangeMerged
      Change_ChangeStateClosed -> EChangeClosed

toETaskData :: TaskData -> ETaskData
toETaskData TaskData {..} =
  let tdTid = toText taskDataTid
      tdTtype = toList $ toText <$> taskDataTtype
      tdChangeUrl = toText taskDataChangeUrl
      tdSeverity = toText taskDataSeverity
      tdPriority = toText taskDataPriority
      tdScore = fromInteger $ toInteger taskDataScore
      tdUrl = toText taskDataUrl
      tdTitle = toText taskDataTitle
      tdPrefix = toText taskDataPrefix
      -- We might get a maybe Timestamp - do not fail if Nothing
      tdUpdatedAt = UTCTimePlus $ maybe defaultDate T.toUTCTime taskDataUpdatedAt
   in ETaskData {..}
  where
    defaultDate = [utctime|1960-01-01 00:00:00|]

runAddDocsBulkOPs ::
  -- | The helper function to create the bulk operation
  (BH.IndexName -> (Value, BH.DocId) -> BH.BulkOperation) ->
  -- | The docs payload
  [(Value, BH.DocId)] ->
  TenantM ()
runAddDocsBulkOPs bulkOp docs = do
  index <- getIndexName
  let stream = V.fromList $ fmap (bulkOp index) docs
  _ <- BH.bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- BH.refreshIndex index
  pure ()

indexDocs :: [(Value, BH.DocId)] -> TenantM ()
indexDocs = runAddDocsBulkOPs toBulkIndex
  where
    -- BulkIndex operation: Create the document, replacing it if it already exists.
    toBulkIndex index (doc, docId) = BH.BulkIndex index docId doc

updateDocs :: [(Value, BH.DocId)] -> TenantM ()
updateDocs = runAddDocsBulkOPs toBulkUpdate
  where
    -- BulkUpdate operation: Update the document, merging the new value with the existing one.
    toBulkUpdate index (doc, docId) = BH.BulkUpdate index docId doc

upsertDocs :: [(Value, BH.DocId)] -> TenantM ()
upsertDocs = runAddDocsBulkOPs toBulkUpsert
  where
    -- BulkUpsert operation: Update the document if it already exists, otherwise insert it.
    toBulkUpsert index (doc, docId) = BH.BulkUpsert index docId (BH.UpsertDoc doc) []

-- | Generated base64 encoding of Text
getBase64Text :: Text -> Text
getBase64Text = decodeUtf8 . B64.encode . encodeUtf8

runScanSearch :: forall a. FromJSON a => BH.Search -> TenantM [a]
runScanSearch search = do
  index <- getIndexName
  results <- scanSearch index
  pure $ catMaybes $ BH.hitSource <$> results
  where
    scanSearch :: (MonadBH m, MonadThrow m) => BH.IndexName -> m [BH.Hit a]
    scanSearch index = BH.scanSearch index search

getChangeDocId :: EChange -> BH.DocId
getChangeDocId change = BH.DocId . toText $ echangeId change

indexChanges :: [EChange] -> TenantM ()
indexChanges changes = indexDocs $ fmap (toDoc . ensureType) changes
  where
    toDoc change = (toJSON change, getChangeDocId change)
    ensureType change = change {echangeType = EChangeDoc}

getEventDocId :: EChangeEvent -> BH.DocId
getEventDocId event = BH.DocId . toStrict $ echangeeventId event

indexEvents :: [EChangeEvent] -> TenantM ()
indexEvents events = indexDocs (fmap toDoc events)
  where
    toDoc ev = (toJSON ev, getEventDocId ev)

statusCheck :: (Int -> c) -> HTTP.Response body -> c
statusCheck prd = prd . NHTS.statusCode . HTTP.responseStatus

isNotFound :: BH.Reply -> Bool
isNotFound = statusCheck (== 404)

checkDocExists :: BH.DocId -> TenantM Bool
checkDocExists docId = do
  index <- getIndexName
  BH.documentExists index docId

getDocumentById :: (FromJSON a) => BH.DocId -> TenantM (Maybe a)
getDocumentById docId = do
  index <- getIndexName
  resp <- BH.getDocument index docId
  if isNotFound resp
    then pure Nothing
    else do
      parsed <- BH.parseEsResponse resp
      case parsed of
        Right cm -> pure . getHit $ BH.foundResult cm
        Left _ -> error "Unable to get parse result"
  where
    getHit (Just (BH.EsResultFound _ cm)) = Just cm
    getHit Nothing = Nothing

getCrawlerMetadataDocId :: Text -> Text -> Text -> BH.DocId
getCrawlerMetadataDocId crawlerName crawlerType crawlerTypeValue =
  BH.DocId . Text.replace "/" "@" $
    Text.intercalate
      "-"
      [ crawlerName,
        crawlerType,
        crawlerTypeValue
      ]

getTDCrawlerMetadataDocId :: Text -> BH.DocId
getTDCrawlerMetadataDocId name = getCrawlerMetadataDocId name "task-data" "issue"

getTDCrawlerMetadata :: Text -> TenantM (Maybe ELKCrawlerMetadata)
getTDCrawlerMetadata name = getDocumentById $ getTDCrawlerMetadataDocId name

setTDCrawlerCommitDate :: Text -> UTCTime -> TenantM ()
setTDCrawlerCommitDate name commitDate = do
  index <- getIndexName
  let elkcmLastCommitAt = commitDate
      elkcmCrawlerType = ""
      elkcmCrawlerTypeValue = ""
      elkcmCrawlerName = ""
      doc = ELKCrawlerMetadata $ ELKCrawlerMetadataObject {..}
      docID = getTDCrawlerMetadataDocId name
  exists <- BH.documentExists index docID
  void $
    if exists
      then BH.updateDocument index BH.defaultIndexDocumentSettings doc docID
      else BH.indexDocument index BH.defaultIndexDocumentSettings doc docID
  void $ BH.refreshIndex index

getTDCrawlerCommitDate :: Text -> Config.Crawler -> TenantM UTCTime
getTDCrawlerCommitDate name crawler = do
  metadata <- getTDCrawlerMetadata name
  let commitDate = elkcmLastCommitAt . elkcmCrawlerMetadata <$> metadata
      currentTS =
        fromMaybe
          (error "Unable to get a valid crawler metadata TS")
          (commitDate <|> parseDateValue (toString (Config.update_since crawler)))
  pure currentTS

getChangesByURL ::
  -- | List of URLs
  [Text] ->
  TenantM [EChange]
getChangesByURL urls = runScanSearch search
  where
    search = BH.mkSearch (Just query) Nothing
    query =
      mkAnd
        [ BH.TermQuery (BH.Term "type" $ toText $ docTypeToText EChangeDoc) Nothing,
          BH.TermsQuery "url" $ fromList urls
        ]

getChangesEventsByURL ::
  -- | List of URLs
  [Text] ->
  TenantM [EChangeEvent]
getChangesEventsByURL urls = runScanSearch search
  where
    search = BH.mkSearch (Just query) Nothing
    query =
      mkAnd
        [ BH.TermsQuery "type" $ fromList eventTypesAsText,
          BH.TermsQuery "url" $ fromList urls
        ]

type HashTable k v = H.BasicHashTable k v

data TaskDataDoc = TaskDataDoc
  { tddId :: LText,
    tddTd :: [ETaskData]
  }
  deriving (Show)

type TaskDataOrphanDoc = TaskDataDoc

getOrphanTaskDataByChangeURL :: [Text] -> TenantM [EChangeOrphanTD]
getOrphanTaskDataByChangeURL urls = do
  index <- getIndexName
  results <- scanSearch index
  pure $ catMaybes $ BH.hitSource <$> results
  where
    scanSearch :: (MonadBH m, MonadThrow m) => BH.IndexName -> m [BH.Hit EChangeOrphanTD]
    scanSearch index = BH.scanSearch index search
    search = BH.mkSearch (Just query) Nothing
    query =
      mkAnd
        [ mkNot [BH.QueryExistsQuery $ BH.FieldName "tasks_data._adopted"],
          mkAnd
            [ BH.TermQuery (BH.Term "type" $ toText $ docTypeToText EOrphanTaskData) Nothing,
              BH.TermsQuery "tasks_data.change_url" $ fromList urls
            ]
        ]

getOrphanTaskDataAndDeclareAdoption :: [Text] -> TenantM [EChangeOrphanTD]
getOrphanTaskDataAndDeclareAdoption urls = do
  oTDs <- getOrphanTaskDataByChangeURL urls
  void $ updateDocs $ toAdoptedDoc <$> oTDs
  pure oTDs
  where
    toAdoptedDoc :: EChangeOrphanTD -> (Value, BH.DocId)
    toAdoptedDoc (EChangeOrphanTD id' _ _) =
      ( toJSON $ EChangeOrphanTDAdopted id' EOrphanTaskData $ ETaskDataAdopted "",
        BH.DocId id'
      )

updateChangesAndEventsFromOrphanTaskData :: [EChange] -> [EChangeEvent] -> TenantM ()
updateChangesAndEventsFromOrphanTaskData changes events = do
  let mapping = uMapping Map.empty getFlatMapping
  adoptedTDs <- getOrphanTaskDataAndDeclareAdoption $ toText <$> Map.keys mapping
  updateDocs $ taskDataDocToBHDoc <$> getTaskDatas adoptedTDs (Map.assocs mapping)
  where
    -- For each change and event extract (changeUrl, object ID)
    getFlatMapping :: [(LText, LText)]
    getFlatMapping =
      ((\c -> (echangeUrl c, echangeId c)) <$> changes)
        <> ((\c -> (echangeeventUrl c, echangeeventId c)) <$> events)
    -- Create a Map where each key (changeUrl) maps a list of object ID
    uMapping :: Map LText [LText] -> [(LText, LText)] -> Map LText [LText]
    uMapping cM fm = case fm of
      [] -> cM
      (x : xs) -> let nM = Map.alter (updateE $ snd x) (fst x) cM in uMapping nM xs
      where
        updateE nE cEs = Just $ maybe [nE] (<> [nE]) cEs
    -- Gather TasksData from matching adopted TD object and create [TaskDataDoc]
    -- for Changes and Events
    getTaskDatas :: [EChangeOrphanTD] -> [(LText, [LText])] -> [TaskDataDoc]
    getTaskDatas adopted assocs = concatMap getTDs assocs
      where
        getTDs :: (LText, [LText]) -> [TaskDataDoc]
        getTDs (url, ids) =
          let mTDs = echangeorphantdTasksData <$> filterByUrl url adopted
           in flip TaskDataDoc mTDs <$> ids
        filterByUrl url = filter (\td -> tdChangeUrl (echangeorphantdTasksData td) == toText url)

taskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
taskDataDocToBHDoc TaskDataDoc {..} =
  (toJSON $ EChangeTD $ Just tddTd, BH.DocId $ toText tddId)

orphanTaskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
orphanTaskDataDocToBHDoc TaskDataDoc {..} =
  let td = head $ fromList tddTd
   in ( toJSON $
          EChangeOrphanTD
            (toText tddId)
            EOrphanTaskData
            td,
        BH.DocId $ toText tddId
      )

taskDataAdd :: [TaskData] -> TenantM ()
taskDataAdd tds = do
  -- extract change URLs from input TDs
  let urls = toText . taskDataChangeUrl <$> tds
  -- get changes that matches those URLs
  changes <- getChangesByURL urls
  -- TODO remove the limit here by using the scan search
  -- get change events that matches those URLs
  changeEvents <- getChangesEventsByURL urls
  -- Init the HashTable that we are going to use as a facility for processing
  changesHT <- liftIO $ initHT changes
  -- Update the HashTable based on incomming TDs and return orphan TDs
  orphanTaskDataDocs <- liftIO $ updateChangesWithTD changesHT
  -- Get TDs from the HashTable
  taskDataDocs <- fmap snd <$> liftIO (H.toList changesHT)
  -- Get the TDs form matching change events
  taskDataDocs' <-
    liftIO $
      fmap catMaybes <$> sequence $
        getTDforEventFromHT changesHT <$> changeEvents
  -- Let's push the data
  updateDocs (taskDataDocToBHDoc <$> taskDataDocs <> taskDataDocs')
  upsertDocs (orphanTaskDataDocToBHDoc <$> orphanTaskDataDocs)
  where
    initHT :: [EChange] -> IO (HashTable LText TaskDataDoc)
    initHT changes = H.fromList $ getMCsTuple <$> changes
      where
        getMCsTuple EChange {echangeUrl, echangeId, echangeTasksData} =
          (echangeUrl, TaskDataDoc echangeId (fromMaybe [] echangeTasksData))

    updateChangesWithTD ::
      -- | The local cache in form of HashMap
      HashTable LText TaskDataDoc ->
      -- | IO action with the list of orphan Task Data
      IO [TaskDataOrphanDoc]
    updateChangesWithTD ht = catMaybes <$> traverse handleTD (toETaskData <$> tds)
      where
        handleTD ::
          -- | The input Task Data we want to append or update
          ETaskData ->
          -- | IO Action with maybe an orphan task data if a matching change does not exists
          IO (Maybe TaskDataOrphanDoc)
        handleTD td = H.mutate ht (toLazy $ tdChangeUrl td) $ \case
          -- Cannot find a change matching this TD -> this TD will be orphan
          Nothing -> (Nothing, Just $ TaskDataDoc {tddId = urlToId $ tdUrl td, tddTd = [td]})
          -- Found a change matching this TD -> update existing TDs with new TD
          Just taskDataDoc -> (Just $ updateTDD taskDataDoc td, Nothing)
          where
            urlToId = toLazy . getBase64Text

        updateTDD ::
          -- | The value of the HashMap we are working on
          TaskDataDoc ->
          -- | The input Task Data we want to append or update
          ETaskData ->
          TaskDataDoc
        updateTDD taskDataDoc td = do
          let changeTDs = tddTd taskDataDoc
              -- The td has been updated so we remove any previous instance
              isOldTD td' = tdUrl td' == tdUrl td
              -- And we cons the new td.
              currentTDs = td : filter (not . isOldTD) changeTDs
           in taskDataDoc {tddTd = currentTDs}

    getTDforEventFromHT ::
      -- | The local cache in form of HashMap
      HashTable LText TaskDataDoc ->
      -- | The ChangeEvent to look for
      EChangeEvent ->
      -- | IO Action returning maybe a TaskData
      IO (Maybe TaskDataDoc)
    getTDforEventFromHT ht changeEvent = do
      mcM <- H.lookup ht $ echangeeventUrl changeEvent
      pure $ case mcM of
        Nothing -> Nothing
        Just mc -> Just $ TaskDataDoc {tddId = echangeeventId changeEvent, tddTd = tddTd mc}

type EntityType = CrawlerPB.CommitInfoRequest_EntityType

getWorkerName :: Config.Crawler -> Text
getWorkerName Config.Crawler {..} = name

getWorkerUpdatedSince :: Config.Crawler -> UTCTime
getWorkerUpdatedSince Config.Crawler {..} =
  fromMaybe
    (error "Invalid date format: Expected format YYYY-mm-dd or YYYY-mm-dd hh:mm:ss UTC")
    $ parseDateValue (toString update_since)

getLastUpdated :: Config.Crawler -> EntityType -> Word32 -> TenantM (Text, UTCTime)
getLastUpdated crawler entity offset = do
  index <- getIndexName
  resp <- fmap BH.hitSource <$> simpleSearch index search
  case nonEmpty (catMaybes resp) of
    Nothing ->
      error
        ( "Unable to find crawler metadata of type:"
            <> getCrawlerTypeAsText entity
            <> " for crawler:"
            <> getWorkerName crawler
        )
    Just xs -> pure $ getRespFromMetadata (last xs)
  where
    search =
      (BH.mkSearch (Just query) Nothing)
        { BH.size = BH.Size (fromInteger . toInteger $ offset + 1),
          BH.sortBody = Just [BH.DefaultSortSpec bhSort]
        }

    bhSort = BH.DefaultSort (BH.FieldName "crawler_metadata.last_commit_at") BH.Ascending Nothing Nothing Nothing Nothing
    query =
      mkAnd
        [ BH.TermQuery (BH.Term "crawler_metadata.crawler_name" (getWorkerName crawler)) Nothing,
          BH.TermQuery (BH.Term "crawler_metadata.crawler_type" (getCrawlerTypeAsText entity)) Nothing
        ]
    getRespFromMetadata (ELKCrawlerMetadata ELKCrawlerMetadataObject {..}) =
      (toStrict elkcmCrawlerTypeValue, elkcmLastCommitAt)

getCrawlerTypeAsText :: EntityType -> Text
getCrawlerTypeAsText entity' = case entity' of
  CrawlerPB.CommitInfoRequest_EntityTypeProject -> "project"
  CrawlerPB.CommitInfoRequest_EntityTypeOrganization -> "organization"
  otherEntity -> error $ "Unsupported Entity: " <> show otherEntity

setOrUpdateLastUpdated :: Bool -> Text -> UTCTime -> Entity -> TenantM ()
setOrUpdateLastUpdated doNotUpdate crawlerName lastUpdatedDate entity = do
  index <- getIndexName
  exists <- BH.documentExists index id'
  when ((exists && not doNotUpdate) || not exists) $ do
    resp <-
      if exists
        then BH.updateDocument index BH.defaultIndexDocumentSettings cm id'
        else BH.indexDocument index BH.defaultIndexDocumentSettings cm id'
    _ <- BH.refreshIndex index
    if BH.isSuccess resp then pure () else error $ "Unable to set Crawler Metadata: " <> show resp
  where
    id' = getId entity
    cm =
      ELKCrawlerMetadata
        { elkcmCrawlerMetadata =
            ELKCrawlerMetadataObject
              (toLazy crawlerName)
              (toLazy $ crawlerType entity)
              (toLazy $ getName entity)
              lastUpdatedDate
        }
    getId entity' = getCrawlerMetadataDocId crawlerName (crawlerType entity') (getName entity')
    crawlerType = \case
      Project _ -> "project"
      Organization _ -> "organization"

setLastUpdated :: Text -> UTCTime -> Entity -> TenantM ()
setLastUpdated = setOrUpdateLastUpdated False

initCrawlerEntities :: [Entity] -> Config.Crawler -> TenantM ()
initCrawlerEntities entities worker = traverse_ run entities
  where
    run = setOrUpdateLastUpdated True (getWorkerName worker) (getWorkerUpdatedSince worker)

getProjectEntityFromCrawler :: Config.Crawler -> [Entity]
getProjectEntityFromCrawler worker = Project <$> Config.getCrawlerProject worker

getOrganizationEntityFromCrawler :: Config.Crawler -> Maybe Entity
getOrganizationEntityFromCrawler worker = Organization <$> Config.getCrawlerOrganization worker

initCrawlerMetadata :: Config.Crawler -> TenantM ()
initCrawlerMetadata crawler =
  initCrawlerEntities
    ( getProjectEntityFromCrawler crawler
        <> maybe [] (: []) (getOrganizationEntityFromCrawler crawler)
    )
    crawler
