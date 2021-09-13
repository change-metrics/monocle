-- | Index management functions such as document mapping and ingest
module Monocle.Backend.Index where

import Data.Aeson
  ( KeyValue ((.=)),
    object,
  )
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.HashTable.IO as H
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
              "task_data"
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

toELKChangeEvent :: ChangeEvent -> ELKChangeEvent
toELKChangeEvent ChangeEvent {..} =
  ELKChangeEvent
    { elkchangeeventId = changeEventId,
      elkchangeeventNumber = fromIntegral changeEventNumber,
      elkchangeeventType = getEventType changeEventType,
      elkchangeeventChangeId = changeEventChangeId,
      elkchangeeventUrl = changeEventUrl,
      elkchangeeventChangedFiles = SimpleFile . changedFilePathPath <$> toList changeEventChangedFiles,
      elkchangeeventRepositoryPrefix = changeEventRepositoryPrefix,
      elkchangeeventRepositoryFullname = changeEventRepositoryFullname,
      elkchangeeventRepositoryShortname = changeEventRepositoryShortname,
      elkchangeeventAuthor = toAuthor changeEventAuthor,
      elkchangeeventOnAuthor = toAuthor changeEventOnAuthor,
      elkchangeeventBranch = changeEventBranch,
      elkchangeeventCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventCreatedAt field is mandatory") changeEventCreatedAt,
      elkchangeeventOnCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventOnCreatedAt field is mandatory") changeEventOnCreatedAt,
      elkchangeeventApproval = case changeEventType of
        Just (ChangeEventTypeChangeReviewed (ChangeReviewedEvent approval)) -> Just $ toList approval
        _anyOtherApprovals -> Nothing,
      elkchangeeventTasksData = Nothing
    }
  where
    getEventType :: Maybe ChangeEventType -> ELKDocType
    getEventType eventTypeM = case eventTypeM of
      Just eventType -> case eventType of
        ChangeEventTypeChangeCreated ChangeCreatedEvent -> ElkChangeCreatedEvent
        ChangeEventTypeChangeCommented ChangeCommentedEvent -> ElkChangeCommentedEvent
        ChangeEventTypeChangeAbandoned ChangeAbandonedEvent -> ElkChangeAbandonedEvent
        ChangeEventTypeChangeReviewed (ChangeReviewedEvent _) -> ElkChangeReviewedEvent
        ChangeEventTypeChangeCommitForcePushed ChangeCommitForcePushedEvent -> ElkChangeCommitForcePushedEvent
        ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent -> ElkChangeCommitPushedEvent
        ChangeEventTypeChangeMerged ChangeMergedEvent -> ElkChangeMergedEvent
      Nothing -> error "changeEventType field is mandatory"

toELKChange :: Change -> ELKChange
toELKChange Change {..} =
  ELKChange
    { elkchangeId = changeId,
      elkchangeType = ElkChange,
      elkchangeTitle = changeTitle,
      elkchangeUrl = changeUrl,
      elkchangeCommitCount = fromInteger . toInteger $ changeCommitCount,
      elkchangeNumber = fromInteger . toInteger $ changeNumber,
      elkchangeChangeId = changeChangeId,
      elkchangeText = changeText,
      elkchangeAdditions = fromInteger $ toInteger changeAdditions,
      elkchangeDeletions = fromInteger $ toInteger changeDeletions,
      elkchangeChangedFilesCount = fromInteger $ toInteger changeChangedFilesCount,
      elkchangeChangedFiles = map toFile $ toList changeChangedFiles,
      elkchangeCommits = map toCommit $ toList changeCommits,
      elkchangeRepositoryPrefix = changeRepositoryPrefix,
      elkchangeRepositoryFullname = changeRepositoryFullname,
      elkchangeRepositoryShortname = changeRepositoryShortname,
      elkchangeAuthor = toAuthor changeAuthor,
      elkchangeMergedBy = toMergedByAuthor <$> changeOptionalMergedBy,
      elkchangeBranch = changeBranch,
      elkchangeTargetBranch = changeTargetBranch,
      elkchangeCreatedAt = T.toUTCTime $ fromMaybe (error "CreatedAt field is mandatory") changeCreatedAt,
      elkchangeMergedAt = toMergedAt <$> changeOptionalMergedAt,
      elkchangeUpdatedAt = T.toUTCTime $ fromMaybe (error "UpdatedAt field is mandatory") changeUpdatedAt,
      elkchangeClosedAt = toClosedAt <$> changeOptionalClosedAt,
      elkchangeState = toState $ fromPBEnum changeState,
      elkchangeDuration = toDuration <$> changeOptionalDuration,
      elkchangeMergeable = changeMergeable,
      elkchangeLabels = toList changeLabels,
      elkchangeAssignees = map toAuthor $ toList $ fmap Just changeAssignees,
      elkchangeApproval = Just $ toList changeApprovals,
      elkchangeDraft = changeDraft,
      elkchangeSelfMerged = toSelfMerged <$> changeOptionalSelfMerged,
      elkchangeTasksData = Nothing
    }
  where
    toFile :: ChangedFile -> File
    toFile ChangedFile {..} =
      File (fromIntegral changedFileAdditions) (fromIntegral changedFileDeletions) changedFilePath
    toCommit :: Monocle.Change.Commit -> Monocle.Backend.Documents.Commit
    toCommit Monocle.Change.Commit {..} =
      Monocle.Backend.Documents.Commit
        { elkcommitSha = commitSha,
          elkcommitAuthor = toAuthor commitAuthor,
          elkcommitCommitter = toAuthor commitCommitter,
          elkcommitAuthoredAt = T.toUTCTime $ fromMaybe (error "AuthoredAt field is mandatory") commitAuthoredAt,
          elkcommitCommittedAt = T.toUTCTime $ fromMaybe (error "CommittedAt field is mandatory") commitCommittedAt,
          elkcommitDeletions = fromIntegral commitDeletions,
          elkcommitAdditions = fromIntegral commitAdditions,
          elkcommitTitle = commitTitle
        }
    toMergedByAuthor (ChangeOptionalMergedByMergedBy m) = toAuthor (Just m)
    toMergedAt (ChangeOptionalMergedAtMergedAt t) = T.toUTCTime t
    toClosedAt (ChangeOptionalClosedAtClosedAt t) = T.toUTCTime t
    toDuration (ChangeOptionalDurationDuration d) = fromInteger $ toInteger d
    toSelfMerged (ChangeOptionalSelfMergedSelfMerged b) = b
    toState cstate = case cstate of
      Change_ChangeStateOpen -> ElkChangeOpen
      Change_ChangeStateMerged -> ElkChangeMerged
      Change_ChangeStateClosed -> ElkChangeClosed

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

-- | Generated base64 encoding of Text
getBase64Text :: Text -> Text
getBase64Text docId = encodeBase64 $ encodeUtf8 docId

runSimpleSearch :: FromJSON a => BH.Search -> Int -> TenantM [a]
runSimpleSearch search size = catMaybes <$> run
  where
    run = do
      index <- getIndexName
      fmap BH.hitSource
        <$> simpleSearch
          index
          (search {BH.size = BH.Size size})

getChangeDocId :: ELKChange -> BH.DocId
getChangeDocId change = BH.DocId . toText $ elkchangeId change

indexChanges :: [ELKChange] -> TenantM ()
indexChanges changes = indexDocs $ fmap (toDoc . ensureType) changes
  where
    toDoc change = (toJSON change, getChangeDocId change)
    ensureType change = change {elkchangeType = ElkChange}

getEventDocId :: ELKChangeEvent -> BH.DocId
getEventDocId event = BH.DocId . toStrict $ elkchangeeventId event

indexEvents :: [ELKChangeEvent] -> TenantM ()
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

-- TODO - need to harmonize Crawlers Metadata ID format
getTDCrawlerMetadataDocId :: Text -> BH.DocId
getTDCrawlerMetadataDocId name = BH.DocId $ "crawler/" <> name <> "/tasks_crawler"

getTDCrawlerMetadata :: Text -> TenantM (Maybe ELKCrawlerMetadata)
getTDCrawlerMetadata name = getDocumentById $ getTDCrawlerMetadataDocId name

setTDCrawlerCommitDate :: Text -> UTCTime -> TenantM ()
setTDCrawlerCommitDate name commitDate = do
  let elkcmLastCommitAt = commitDate
      elkcmCrawlerType = ""
      elkcmCrawlerTypeValue = ""
      elkcmCrawlerName = ""
      doc = ELKCrawlerMetadata $ ELKCrawlerMetadataObject {..}
  indexDocs [(toJSON doc, getTDCrawlerMetadataDocId name)]

getTDCrawlerCommitDate :: Text -> TenantM (Maybe UTCTime)
getTDCrawlerCommitDate name = do
  metadata <- getTDCrawlerMetadata name
  pure $ elkcmLastCommitAt . elkcmCrawlerMetadata <$> metadata

getChangesByURL ::
  -- | List of URLs
  [Text] ->
  -- | Page size
  Int ->
  TenantM [ELKChange]
getChangesByURL urls = runSimpleSearch search
  where
    search = BH.mkSearch (Just query) Nothing
    query =
      mkAnd
        [ BH.TermQuery (BH.Term "type" "Change") Nothing,
          BH.TermsQuery "url" $ fromList urls
        ]

-- TODO: previous implementation was using scan an not limit
getChangesEventsByURL ::
  -- | List of URLs
  [Text] ->
  -- | Page size
  Int ->
  TenantM [ELKChangeEvent]
getChangesEventsByURL urls = runSimpleSearch search
  where
    search = BH.mkSearch (Just query) Nothing
    query =
      mkAnd
        [ BH.TermsQuery "type" $ fromList eventTypesAsText,
          BH.TermsQuery "url" $ fromList urls
        ]

type HashTable k v = H.BasicHashTable k v

data TaskDataDoc = TaskDataDoc {tddId :: LText, tddTd :: [ELKTaskData]} deriving (Show)

type TaskDataOrphanDoc = TaskDataDoc

taskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
taskDataDocToBHDoc TaskDataDoc {..} =
  (toJSON $ ELKChangeTD $ Just tddTd, BH.DocId $ toText tddId)

orphanTaskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
orphanTaskDataDocToBHDoc TaskDataDoc {..} =
  (toJSON $ ELKChangeOrphanTD ElkOrphanTaskData (Just tddTd), BH.DocId $ toText tddId)

taskDataAdd :: [TaskData] -> TenantM ()
taskDataAdd tds = do
  -- extract change URLs from input TDs
  let urls = toText . taskDataChangeUrl <$> tds
      inputTaskDataLimit = 500
  -- get changes that matches those URLs
  changes <- getChangesByURL urls inputTaskDataLimit
  -- TODO remove the limit here by using the scan search
  -- get change events that matches those URLs
  changeEvents <- getChangesEventsByURL urls 10000
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
  indexDocs (orphanTaskDataDocToBHDoc <$> orphanTaskDataDocs)
  where
    initHT :: [ELKChange] -> IO (HashTable LText TaskDataDoc)
    initHT changes = H.fromList $ getMCsTuple <$> changes
      where
        getMCsTuple ELKChange {elkchangeUrl, elkchangeId, elkchangeTasksData} =
          (elkchangeUrl, TaskDataDoc elkchangeId (fromMaybe [] elkchangeTasksData))

    updateChangesWithTD ::
      -- | The local cache in form of HashMap
      HashTable LText TaskDataDoc ->
      -- | IO action with the list of orphan Task Data
      IO [TaskDataOrphanDoc]
    updateChangesWithTD ht = catMaybes <$> traverse handleTD (toELKTaskData <$> tds)
      where
        handleTD ::
          -- | The input Task Data we want to append or update
          ELKTaskData ->
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
          ELKTaskData ->
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
      ELKChangeEvent ->
      -- | IO Action returning maybe a TaskData
      IO (Maybe TaskDataDoc)
    getTDforEventFromHT ht changeEvent = do
      mcM <- H.lookup ht $ elkchangeeventUrl changeEvent
      pure $ case mcM of
        Nothing -> Nothing
        Just mc -> Just $ TaskDataDoc {tddId = elkchangeeventId changeEvent, tddTd = tddTd mc}

    toELKTaskData :: TaskData -> ELKTaskData
    toELKTaskData TaskData {..} =
      let tdTid = toText taskDataTid
          tdTtype = toList $ toText <$> taskDataTtype
          tdChangeUrl = toText taskDataChangeUrl
          tdSeverity = toText taskDataSeverity
          tdPriority = toText taskDataPriority
          tdScore = fromInteger $ toInteger taskDataScore
          tdUrl = toText taskDataUrl
          tdTitle = toText taskDataTitle
       in ELKTaskData {..}

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
