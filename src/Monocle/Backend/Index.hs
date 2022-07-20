-- | Index management functions such as document mapping and ingest
module Monocle.Backend.Index where

import Crypto.Hash.SHA256 (hash)
import Data.Aeson (object)
import Data.Aeson.Types (Pair)
import Data.ByteString.Base64 qualified as B64
import Data.HashTable.IO qualified as H
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Database.Bloodhound.Raw qualified as BHR
import Google.Protobuf.Timestamp as T
import Monocle.Backend.Documents
import Monocle.Backend.Queries qualified as Q
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging (Entity (..), getEntityName)
import Monocle.Prelude
import Monocle.Protob.Change qualified as ChangePB
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Search qualified as SearchPB (Order (..), Order_Direction (..), TaskData (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status qualified as NHTS
import Proto3.Suite.Types qualified as PT (Enumerated (..))
import Streaming qualified as S (chunksOf)
import Streaming.Prelude qualified as S
import Streaming.Prelude qualified as Streaming

data ConfigIndexMapping = ConfigIndexMapping deriving (Eq, Show)

instance ToJSON ConfigIndexMapping where
  toJSON ConfigIndexMapping =
    object
      [ "properties"
          .= object
            ["version" .= object ["type" .= ("integer" :: Text)]]
      ]

data ChangesIndexMapping = ChangesIndexMapping deriving (Eq, Show)

data AuthorIndexMapping = AuthorIndexMapping deriving (Eq, Show)

data CachedAuthorIndexMapping = CachedAuthorIndexMapping deriving (Eq, Show)

data AuthorMapping = AuthorMapping deriving (Eq, Show)

instance ToJSON AuthorMapping where
  toJSON AuthorMapping =
    object
      [ "uid" .= object ["type" .= ("keyword" :: Text)],
        "muid" .= object ["type" .= ("keyword" :: Text)]
      ]

instance ToJSON AuthorIndexMapping where
  toJSON AuthorIndexMapping =
    object ["properties" .= AuthorMapping]

cachedAuthorFields :: [Pair]
cachedAuthorFields = ["cached_author_muid" .= TextAndKWMapping]

instance ToJSON CachedAuthorIndexMapping where
  toJSON CachedAuthorIndexMapping =
    object ["properties" .= object cachedAuthorFields]

data DateIndexMapping = DateIndexMapping deriving (Eq, Show)

instance ToJSON DateIndexMapping where
  toJSON DateIndexMapping =
    object
      [ "type" .= ("date" :: Text),
        "format" .= ("date_time_no_millis" :: Text)
      ]

data TextAndKWMapping = TextAndKWMapping deriving (Eq, Show)

instance ToJSON TextAndKWMapping where
  toJSON TextAndKWMapping =
    object
      [ "type" .= ("text" :: Text),
        "fields"
          .= object
            [ "keyword"
                .= object
                  [ "type" .= ("keyword" :: Text),
                    "ignore_above" .= (8191 :: Int)
                  ]
            ]
      ]

data KWMapping = KWMapping deriving (Eq, Show)

instance ToJSON KWMapping where
  toJSON KWMapping = object ["type" .= ("keyword" :: Text)]

data IntegerMapping = IntegerMapping deriving (Eq, Show)

instance ToJSON IntegerMapping where
  toJSON IntegerMapping = object ["type" .= ("integer" :: Text)]

data BoolMapping = BoolMapping deriving (Eq, Show)

instance ToJSON BoolMapping where
  toJSON BoolMapping = object ["type" .= ("boolean" :: Text)]

instance ToJSON ChangesIndexMapping where
  toJSON ChangesIndexMapping =
    object
      [ "properties"
          .= object
            ( [ "id" .= KWMapping,
                "type" .= KWMapping,
                "number" .= KWMapping,
                "change_id" .= KWMapping,
                "title" .= TextAndKWMapping,
                "text" .= TextAndKWMapping,
                "url" .= KWMapping,
                "commit_count" .= IntegerMapping,
                "additions" .= IntegerMapping,
                "deletions" .= IntegerMapping,
                "change_files_count" .= IntegerMapping,
                "changed_files"
                  .= object
                    [ "properties"
                        .= object
                          [ "additions" .= IntegerMapping,
                            "deletions" .= IntegerMapping,
                            "path" .= KWMapping
                          ]
                    ],
                "commits"
                  .= object
                    [ "properties"
                        .= object
                          [ "sha" .= KWMapping,
                            "author" .= AuthorIndexMapping,
                            "committer" .= AuthorIndexMapping,
                            "authored_at" .= DateIndexMapping,
                            "committed_at" .= DateIndexMapping,
                            "additions" .= IntegerMapping,
                            "deletions" .= IntegerMapping,
                            "title" .= object ["type" .= ("text" :: Text)]
                          ]
                    ],
                "repository_prefix" .= KWMapping,
                "repository_fullname" .= KWMapping,
                "repository_shortname" .= KWMapping,
                "author" .= AuthorIndexMapping,
                "on_author" .= AuthorIndexMapping,
                "committer" .= AuthorIndexMapping,
                "merged_by" .= AuthorIndexMapping,
                "branch" .= KWMapping,
                "target_branch" .= KWMapping,
                "created_at" .= DateIndexMapping,
                "on_created_at" .= DateIndexMapping,
                "merged_at" .= DateIndexMapping,
                "updated_at" .= DateIndexMapping,
                "closed_at" .= DateIndexMapping,
                "state" .= KWMapping,
                "duration" .= IntegerMapping,
                "mergeable" .= KWMapping,
                "labels" .= KWMapping,
                "assignees"
                  .= object
                    [ "type" .= ("nested" :: Text),
                      "properties" .= AuthorMapping
                    ],
                "approval" .= KWMapping,
                "draft" .= BoolMapping,
                "self_merged" .= BoolMapping,
                "crawler_metadata"
                  .= object
                    [ "properties"
                        .= object
                          [ "crawler_name" .= KWMapping,
                            "crawler_type" .= KWMapping,
                            "crawler_type_value" .= KWMapping,
                            "last_commit_at" .= DateIndexMapping,
                            "last_post_at" .= DateIndexMapping,
                            "total_docs_posted" .= IntegerMapping,
                            "total_changes_updated" .= IntegerMapping,
                            "total_change_events_updated" .= IntegerMapping,
                            "total_orphans_updated" .= IntegerMapping
                          ]
                    ],
                "tasks_data"
                  .= object
                    [ "properties"
                        .= object
                          [ "tid" .= KWMapping,
                            "ttype" .= KWMapping,
                            "crawler_name" .= KWMapping,
                            "updated_at" .= DateIndexMapping,
                            "change_url" .= KWMapping,
                            "severity" .= KWMapping,
                            "priority" .= KWMapping,
                            "score" .= IntegerMapping,
                            "url" .= KWMapping,
                            "prefix" .= KWMapping,
                            "title" .= TextAndKWMapping,
                            "_adopted" .= BoolMapping
                          ]
                    ]
              ]
                <> cachedAuthorFields
            )
      ]

createIndex :: (BH.MonadBH m, ToJSON mapping, MonadFail m) => BH.IndexName -> mapping -> m ()
createIndex indexName mapping = do
  _respCI <- BH.createIndex indexSettings indexName
  -- print respCI
  _respPM <- BH.putMapping indexName mapping
  -- print respPM
  True <- BH.indexExists indexName
  pure ()
  where
    indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0) BH.defaultIndexMappingsLimits

configVersion :: ConfigVersion
configVersion = ConfigVersion 3

configIndex :: BH.IndexName
configIndex = BH.IndexName "monocle.config"

configDoc :: BH.DocId
configDoc = BH.DocId "config"

-- | Upgrade to config v1 (migrate legacy GH crawler to the new API)
-- | This function looks for GitHub project crawler metadata docs and reset the
-- | lastCommitAt to the lastUpdatedAt date of the most recent change of the repository.
upgradeConfigV1 :: QueryM ()
upgradeConfigV1 = do
  indexName <- getIndexName
  logMessage $ "Applying migration to schema V1 on workspace " <> show indexName
  QueryWorkspace ws <- asks tenant
  -- Get GitHub crawler names
  let ghCrawlerNames = getGHCrawlerNames ws
  -- Get all the GH crawler project metadata.
  ghCrawlerMD <- traverse getProjectCrawlerMDByName ghCrawlerNames
  -- Keep the one that have the default starting date
  let ghCrawlerMDToReset = filter (isCrawlerLastCommitAtIsDefault ws) $ concat ghCrawlerMD
  -- Update the last_commit_at from the age of the most recent update
  traverse_ setLastUpdatedDate ghCrawlerMDToReset
  where
    getGHCrawlerNames :: Config.Index -> [Text]
    getGHCrawlerNames ws =
      let isGHProvider crawler = case Config.provider crawler of
            Config.GithubProvider _ -> True
            _otherwise -> False
       in Config.getCrawlerName
            <$> filter isGHProvider (Config.crawlers ws)
    getProjectCrawlerMDByName :: Text -> QueryM [ECrawlerMetadata]
    getProjectCrawlerMDByName crawlerName = do
      let entity = CrawlerPB.EntityEntityProjectName ""
          search = BH.mkSearch (Just $ crawlerMDQuery entity crawlerName) Nothing
      index <- getIndexName
      resp <- fmap BH.hitSource <$> simpleSearch index search
      pure $ catMaybes resp
    isCrawlerLastCommitAtIsDefault :: Config.Index -> ECrawlerMetadata -> Bool
    isCrawlerLastCommitAtIsDefault
      ws
      ( ECrawlerMetadata
          ECrawlerMetadataObject {ecmCrawlerName, ecmLastCommitAt}
        ) =
        case Config.lookupCrawler ws (from ecmCrawlerName) of
          Nothing -> False
          Just crawler -> getWorkerUpdatedSince crawler == ecmLastCommitAt
    setLastUpdatedDate :: ECrawlerMetadata -> QueryM ()
    setLastUpdatedDate
      (ECrawlerMetadata ECrawlerMetadataObject {ecmCrawlerName, ecmCrawlerTypeValue}) = do
        lastUpdatedDateM <- getLastUpdatedDate $ from ecmCrawlerTypeValue
        case lastUpdatedDateM of
          Nothing -> pure ()
          Just lastUpdatedAt ->
            setLastUpdated
              (from ecmCrawlerName)
              lastUpdatedAt
              $ Project . from $ ecmCrawlerTypeValue

upgradeConfigV2 :: QueryM ()
upgradeConfigV2 = do
  indexName <- getIndexName
  logMessage $ "Applying migration to schema V2 on workspace " <> show indexName
  void $ BH.putMapping indexName CachedAuthorIndexMapping
  added <- populateAuthorCache
  logMessage $ "Authors cache populated with " <> show added <> " Monocle uids"

-- | Add self_merged data to event of type ChangeMergedEvent
upgradeConfigV3 :: QueryM Int
upgradeConfigV3 = do
  indexName <- getIndexName
  logMessage $ "Applying migration to schema V3 on workspace " <> show indexName
  count <-
    withQuery eventQuery $
      scanEvents
        & ( Streaming.mapMaybe updateEvent
              >>> Streaming.map (mkEventBulkUpdate indexName)
              >>> bulkStream
          )
  logMessage $ "Migration to schema V3 affected " <> show count <> " documents"
  pure count
  where
    scanEvents :: Stream (Of EChangeEvent) QueryM ()
    scanEvents = Q.scanSearchHit
    eventQuery = mkQuery [Q.documentType EChangeMergedEvent]
    updateEvent :: EChangeEvent -> Maybe EChangeEvent
    updateEvent se@EChangeEvent {..}
      | echangeeventAuthor == Just echangeeventOnAuthor = Just $ se {echangeeventSelfMerged = Just True}
      | otherwise = Nothing
    mkEventBulkUpdate :: BH.IndexName -> EChangeEvent -> BulkOperation
    mkEventBulkUpdate indexName ev =
      BulkUpdate indexName (getEventDocId ev) $ toJSON ev

upgrades :: [(ConfigVersion, QueryM ())]
upgrades =
  [ (ConfigVersion 1, upgradeConfigV1),
    (ConfigVersion 2, upgradeConfigV2),
    (ConfigVersion 3, void upgradeConfigV3)
  ]

newtype ConfigVersion = ConfigVersion Integer deriving (Eq, Show, Ord)

-- | Extract the `version` attribute of an Aeson object value
--
-- >>> getVersion (object ["version" .= (42 :: Int)])
-- ConfigVersion 42
-- >>> getVersion (object [])
-- ConfigVersion 0
getVersion :: Value -> ConfigVersion
getVersion = ConfigVersion . fromMaybe 0 . preview (_Object . at "version" . traverse . _Integer)

-- | Set the `version` attribute of an Aeson object
--
-- >>> setVersion (ConfigVersion 23) (object ["version" .= (22 :: Int)])
-- Object (fromList [("version",Number 23.0)])
-- >>> setVersion (ConfigVersion 42) (object [])
-- Object (fromList [("version",Number 42.0)])
setVersion :: ConfigVersion -> Value -> Value
setVersion (ConfigVersion v) = set (_Object . at "version") (Just . Number . fromInteger $ v)

getConfigVersion :: QueryM (ConfigVersion, Value)
getConfigVersion = do
  QueryConfig _ <- asks tenant
  currentConfig <- fromMaybe (object []) <$> getDocumentById' configIndex configDoc
  pure (getVersion currentConfig, currentConfig)

ensureConfigIndex :: QueryM ()
ensureConfigIndex = do
  QueryConfig conf <- asks tenant

  -- Ensure index and index mapping
  createIndex configIndex ConfigIndexMapping

  -- Get current config version
  (currentVersion, currentConfig) <- getConfigVersion

  -- Apply upgrade processes
  traverse_
    ( \(version, procedure) ->
        when (currentVersion < version) $
          traverseWorkspace procedure conf
    )
    upgrades

  -- Write new config version in config index
  let newConfig = setVersion configVersion currentConfig
  void $ BH.indexDocument configIndex BH.defaultIndexDocumentSettings newConfig configDoc
  logMessage $ "Ensure schema version to " <> show configVersion
  where
    -- traverseWorkspace replace the QueryEnv tenant attribute from QueryConfig to QueryWorkspace
    traverseWorkspace action conf = do
      traverse_ (\ws -> local (setTenant ws) action) (Config.getWorkspaces conf)
    setTenant ws e = e {tenant = QueryWorkspace ws}

ensureIndexSetup :: QueryM ()
ensureIndexSetup = do
  indexName <- getIndexName
  logMessage $ "Ensure workspace " <> show indexName
  createIndex indexName ChangesIndexMapping
  BHR.settings indexName (object ["index" .= object ["max_regex_length" .= (50_000 :: Int)]])

ensureIndexCrawlerMetadata :: QueryM ()
ensureIndexCrawlerMetadata = do
  config <- getIndexConfig
  traverse_ initCrawlerMetadata $ Config.crawlers config

withRefresh :: HasCallStack => QueryM BH.Reply -> QueryM ()
withRefresh action = do
  index <- getIndexName
  resp <- action
  unless (BH.isSuccess resp) (error $ "Unable to add or update: " <> show resp)
  refreshResp <- BH.refreshIndex index
  unless (BH.isSuccess refreshResp) (error $ "Unable to refresh index: " <> show resp)

ensureIndex :: QueryM ()
ensureIndex = do
  ensureIndexSetup
  ensureIndexCrawlerMetadata

removeIndex :: QueryM ()
removeIndex = do
  indexName <- getIndexName
  _resp <- BH.deleteIndex indexName
  False <- BH.indexExists indexName
  pure ()

toAuthor :: Maybe ChangePB.Ident -> Monocle.Backend.Documents.Author
toAuthor (Just ChangePB.Ident {..}) =
  Monocle.Backend.Documents.Author
    { authorMuid = identMuid,
      authorUid = identUid
    }
toAuthor Nothing =
  Monocle.Backend.Documents.Author
    "backend-ghost"
    "backend-ghost"

toEChangeEvent :: ChangePB.ChangeEvent -> EChangeEvent
toEChangeEvent ChangePB.ChangeEvent {..} =
  EChangeEvent
    { echangeeventId = changeEventId,
      echangeeventNumber = fromIntegral changeEventNumber,
      echangeeventType = eType,
      echangeeventChangeId = changeEventChangeId,
      echangeeventUrl = changeEventUrl,
      echangeeventChangedFiles = SimpleFile . ChangePB.changedFilePathPath <$> toList changeEventChangedFiles,
      echangeeventRepositoryPrefix = changeEventRepositoryPrefix,
      echangeeventRepositoryFullname = changeEventRepositoryFullname,
      echangeeventRepositoryShortname = changeEventRepositoryShortname,
      echangeeventAuthor = Just author,
      echangeeventOnAuthor = onAuthor,
      echangeeventSelfMerged = case eType of
        EChangeMergedEvent -> Just $ onAuthor == author
        _ -> Nothing,
      echangeeventBranch = changeEventBranch,
      echangeeventLabels = Just . toList $ changeEventLabels,
      echangeeventCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventCreatedAt field is mandatory") changeEventCreatedAt,
      echangeeventOnCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventOnCreatedAt field is mandatory") changeEventOnCreatedAt,
      echangeeventApproval = case changeEventType of
        Just (ChangePB.ChangeEventTypeChangeReviewed (ChangePB.ChangeReviewedEvent approval)) -> Just $ toList approval
        _anyOtherApprovals -> Nothing,
      echangeeventTasksData = Nothing
    }
  where
    author = toAuthor changeEventAuthor
    onAuthor = toAuthor changeEventOnAuthor
    eType = getEventType changeEventType

getEventType :: Maybe ChangePB.ChangeEventType -> EDocType
getEventType eventTypeM = case eventTypeM of
  Just eventType -> case eventType of
    ChangePB.ChangeEventTypeChangeCreated ChangePB.ChangeCreatedEvent -> EChangeCreatedEvent
    ChangePB.ChangeEventTypeChangeCommented ChangePB.ChangeCommentedEvent -> EChangeCommentedEvent
    ChangePB.ChangeEventTypeChangeAbandoned ChangePB.ChangeAbandonedEvent -> EChangeAbandonedEvent
    ChangePB.ChangeEventTypeChangeReviewed (ChangePB.ChangeReviewedEvent _) -> EChangeReviewedEvent
    ChangePB.ChangeEventTypeChangeCommitForcePushed ChangePB.ChangeCommitForcePushedEvent -> EChangeCommitForcePushedEvent
    ChangePB.ChangeEventTypeChangeCommitPushed ChangePB.ChangeCommitPushedEvent -> EChangeCommitPushedEvent
    ChangePB.ChangeEventTypeChangeMerged ChangePB.ChangeMergedEvent -> EChangeMergedEvent
  Nothing -> error "changeEventType field is mandatory"

toETaskData :: Text -> SearchPB.TaskData -> ETaskData
toETaskData crawlerName SearchPB.TaskData {..} =
  let tdTid = from taskDataTid
      tdCrawlerName = Just crawlerName
      tdTtype = toList $ from <$> taskDataTtype
      tdChangeUrl = from taskDataChangeUrl
      tdSeverity = from taskDataSeverity
      tdPriority = from taskDataPriority
      tdScore = fromInteger $ toInteger taskDataScore
      tdUrl = from taskDataUrl
      tdTitle = from taskDataTitle
      tdPrefix = Just $ from taskDataPrefix
      -- We might get a maybe Timestamp - do not fail if Nothing
      tdUpdatedAt = toMonocleTime $ maybe defaultDate T.toUTCTime taskDataUpdatedAt
   in ETaskData {..}
  where
    defaultDate = [utctime|1960-01-01 00:00:00|]

-- | Apply a stream of bulk operation by chunk
bulkStream :: Stream (Of BH.BulkOperation) QueryM () -> QueryM Int
bulkStream s = do
  (count :> _) <- S.sum . S.mapM callBulk . S.mapped S.toList . S.chunksOf 500 $ s
  when (count > 0) $
    -- TODO: check for refresh errors ?
    void $ BH.refreshIndex =<< getIndexName
  pure count
  where
    callBulk :: [BH.BulkOperation] -> QueryM Int
    callBulk ops = do
      let vector = V.fromList ops
      _ <- BH.bulk vector
      -- TODO: check for error
      pure $ V.length vector

runAddDocsBulkOPs ::
  -- | The helper function to create the bulk operation
  (BH.IndexName -> (Value, BH.DocId) -> BH.BulkOperation) ->
  -- | The docs payload
  [(Value, BH.DocId)] ->
  QueryM ()
runAddDocsBulkOPs bulkOp docs = do
  index <- getIndexName
  let stream = V.fromList $ fmap (bulkOp index) docs
  _ <- BH.bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- BH.refreshIndex index
  pure ()

indexDocs :: [(Value, BH.DocId)] -> QueryM ()
indexDocs = runAddDocsBulkOPs toBulkIndex
  where
    -- BulkIndex operation: Create the document, replacing it if it already exists.
    toBulkIndex index (doc, docId) = BH.BulkIndex index docId doc

updateDocs :: [(Value, BH.DocId)] -> QueryM ()
updateDocs = runAddDocsBulkOPs toBulkUpdate
  where
    -- BulkUpdate operation: Update the document, merging the new value with the existing one.
    toBulkUpdate index (doc, docId) = BH.BulkUpdate index docId doc

upsertDocs :: [(Value, BH.DocId)] -> QueryM ()
upsertDocs = runAddDocsBulkOPs toBulkUpsert
  where
    -- BulkUpsert operation: Update the document if it already exists, otherwise insert it.
    toBulkUpsert index (doc, docId) = BH.BulkUpsert index docId (BH.UpsertDoc doc) []

-- | Generate a Text suitable for ElasticSearch Document ID from Text
getDocID :: Text -> Text
getDocID = decodeUtf8 . B64.encode . hash . encodeUtf8

-- | Generate an DocID from Text
getBHDocID :: Text -> BH.DocId
getBHDocID = BH.DocId . getDocID

-- | A simple scan search that loads all the results in memory
runScanSearch :: forall a. FromJSONField a => BH.Query -> QueryM [a]
runScanSearch query = withQuery (mkQuery [query]) Q.scanSearchSimple

getChangeDocId :: EChange -> BH.DocId
getChangeDocId change = BH.DocId . from $ echangeId change

indexChanges :: [EChange] -> QueryM ()
indexChanges changes = indexDocs $ fmap (toDoc . ensureType) changes
  where
    toDoc change = (toJSON change, getChangeDocId change)
    ensureType change = change {echangeType = EChangeDoc}

getEventDocId :: EChangeEvent -> BH.DocId
getEventDocId event = BH.DocId . from $ echangeeventId event

indexEvents :: [EChangeEvent] -> QueryM ()
indexEvents events = indexDocs (fmap toDoc events)
  where
    toDoc ev = (toJSON ev, getEventDocId ev)

statusCheck :: (Int -> c) -> HTTP.Response body -> c
statusCheck prd = prd . NHTS.statusCode . HTTP.responseStatus

isNotFound :: BH.Reply -> Bool
isNotFound = statusCheck (== 404)

checkDocExists :: BH.DocId -> QueryM Bool
checkDocExists docId = do
  index <- getIndexName
  BH.documentExists index docId

getDocumentById' :: (BH.MonadBH m, FromJSON a, MonadThrow m) => BH.IndexName -> BH.DocId -> m (Maybe a)
getDocumentById' index docId = do
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

getDocumentById :: FromJSON a => BH.DocId -> QueryM (Maybe a)
getDocumentById docId = do
  index <- getIndexName
  getDocumentById' index docId

getChangeById :: BH.DocId -> QueryM (Maybe EChange)
getChangeById = getDocumentById

getChangeEventById :: BH.DocId -> QueryM (Maybe EChangeEvent)
getChangeEventById = getDocumentById

getCrawlerMetadataDocId :: Text -> Text -> Text -> BH.DocId
getCrawlerMetadataDocId crawlerName crawlerType crawlerTypeValue =
  BH.DocId . Text.replace "/" "@" $
    Text.intercalate
      "-"
      [ crawlerName,
        crawlerType,
        crawlerTypeValue
      ]

getChangesByURL ::
  -- | List of URLs
  [Text] ->
  BH.Query
getChangesByURL urls = query
  where
    query =
      mkAnd
        [ BH.TermQuery (BH.Term "type" $ from EChangeDoc) Nothing,
          BH.TermsQuery "url" $ fromList urls
        ]

getChangesEventsByURL ::
  -- | List of URLs
  [Text] ->
  BH.Query
getChangesEventsByURL urls = query
  where
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

getOrphanTaskDataByChangeURL :: [Text] -> QueryM [EChangeOrphanTD]
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
            [ BH.TermQuery (BH.Term "type" $ from EOrphanTaskData) Nothing,
              BH.TermsQuery "tasks_data.change_url" $ fromList urls
            ]
        ]

getOrphanTaskDataAndDeclareAdoption :: [Text] -> QueryM [EChangeOrphanTD]
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

updateChangesAndEventsFromOrphanTaskData :: [EChange] -> [EChangeEvent] -> QueryM ()
updateChangesAndEventsFromOrphanTaskData changes events = do
  let mapping = uMapping Map.empty getFlatMapping
  adoptedTDs <- getOrphanTaskDataAndDeclareAdoption $ from <$> Map.keys mapping
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
    getTaskDatas adopted = concatMap getTDs
      where
        getTDs :: (LText, [LText]) -> [TaskDataDoc]
        getTDs (url, ids) =
          let mTDs = echangeorphantdTasksData <$> filterByUrl url adopted
           in flip TaskDataDoc mTDs <$> ids
        filterByUrl url = filter (\td -> tdChangeUrl (echangeorphantdTasksData td) == from url)

taskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
taskDataDocToBHDoc TaskDataDoc {..} =
  (toJSON $ EChangeTD $ Just tddTd, BH.DocId $ from tddId)

orphanTaskDataDocToBHDoc :: TaskDataDoc -> (Value, BH.DocId)
orphanTaskDataDocToBHDoc TaskDataDoc {..} =
  let td = head $ fromList tddTd
   in ( toJSON $
          EChangeOrphanTD
            (from tddId)
            EOrphanTaskData
            td,
        BH.DocId $ from tddId
      )

taskDataAdd :: Text -> [SearchPB.TaskData] -> QueryM ()
taskDataAdd crawlerName tds = do
  -- extract change URLs from input TDs
  let urls = from . SearchPB.taskDataChangeUrl <$> tds
  -- get changes that matches those URLs
  changes <- runScanSearch $ getChangesByURL urls
  -- get change events that matches those URLs
  changeEvents <- runScanSearch $ getChangesEventsByURL urls
  -- Init the HashTable that we are going to use as a facility for processing
  changesHT <- liftIO $ initHT changes
  -- Update the HashTable based on incomming TDs and return orphan TDs
  orphanTaskDataDocs <- liftIO $ updateChangesWithTD changesHT
  -- Get TDs from the HashTable
  taskDataDocs <- fmap snd <$> liftIO (H.toList changesHT)
  -- Get TDs from matching change events
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
      -- The local cache in form of HashMap
      HashTable LText TaskDataDoc ->
      -- IO action with the list of orphan Task Data
      IO [TaskDataOrphanDoc]
    updateChangesWithTD ht = catMaybes <$> traverse handleTD (toETaskData crawlerName <$> tds)
      where
        handleTD ::
          -- The input Task Data we want to append or update
          ETaskData ->
          -- IO Action with maybe an orphan task data if a matching change does not exists
          IO (Maybe TaskDataOrphanDoc)
        handleTD td = H.mutate ht (from $ tdChangeUrl td) $ \case
          -- Cannot find a change matching this TD -> this TD will be orphan
          Nothing -> (Nothing, Just $ TaskDataDoc {tddId = from $ getTDId td, tddTd = [td]})
          -- Found a change matching this TD -> update existing TDs with new TD
          Just taskDataDoc -> (Just $ updateTDD taskDataDoc td, Nothing)
          where
            getTDId ETaskData {..} = let rawId = tdUrl <> tdChangeUrl in getDocID rawId

        updateTDD ::
          -- The value of the HashMap we are working on
          TaskDataDoc ->
          -- The input Task Data we want to append or update
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
      -- The local cache in form of HashMap
      HashTable LText TaskDataDoc ->
      -- The ChangeEvent to look for
      EChangeEvent ->
      -- IO Action returning maybe a TaskData
      IO (Maybe TaskDataDoc)
    getTDforEventFromHT ht changeEvent = do
      mcM <- H.lookup ht $ echangeeventUrl changeEvent
      pure $ case mcM of
        Nothing -> Nothing
        Just mc -> Just $ TaskDataDoc {tddId = echangeeventId changeEvent, tddTd = tddTd mc}

type EntityType = CrawlerPB.EntityEntity

getWorkerName :: Config.Crawler -> Text
getWorkerName Config.Crawler {..} = name

getWorkerUpdatedSince :: Config.Crawler -> UTCTime
getWorkerUpdatedSince Config.Crawler {..} =
  fromMaybe
    (error "Invalid date format: Expected format YYYY-mm-dd or YYYY-mm-dd hh:mm:ss UTC")
    $ parseDateValue (from update_since)

crawlerMDQuery :: EntityType -> Text -> BH.Query
crawlerMDQuery entity crawlerName =
  mkAnd
    [ BH.TermQuery (BH.Term "crawler_metadata.crawler_name" crawlerName) Nothing,
      BH.TermQuery (BH.Term "crawler_metadata.crawler_type" (getCrawlerTypeAsText entity)) Nothing
    ]

getLastUpdated :: Config.Crawler -> EntityType -> Word32 -> QueryM (Maybe (Text, UTCTime))
getLastUpdated crawler entity offset = do
  index <- getIndexName
  resp <- fmap BH.hitSource <$> simpleSearch index search
  case nonEmpty (catMaybes resp) of
    Nothing -> pure Nothing
    Just xs ->
      if length xs == from size
        then pure . Just $ getRespFromMetadata (last xs)
        else pure Nothing
  where
    size = offset + 1
    search =
      (BH.mkSearch (Just $ crawlerMDQuery entity crawlerName) Nothing)
        { BH.size = BH.Size $ from size,
          BH.sortBody = Just [BH.DefaultSortSpec bhSort]
        }

    bhSort = BH.DefaultSort (BH.FieldName "crawler_metadata.last_commit_at") BH.Ascending Nothing Nothing Nothing Nothing
    getRespFromMetadata (ECrawlerMetadata ECrawlerMetadataObject {..}) =
      (from ecmCrawlerTypeValue, ecmLastCommitAt)
    crawlerName = getWorkerName crawler

-- | The following entityRequest are a bit bizarre, this is because we are re-using
-- the entity info response defined in protobuf. When requesting the last updated, we provide
-- an empty entity.
entityRequestProject, entityRequestOrganization, entityRequestTaskData :: CrawlerPB.EntityEntity
entityRequestProject = CrawlerPB.EntityEntityProjectName ""
entityRequestOrganization = CrawlerPB.EntityEntityOrganizationName ""
entityRequestTaskData = CrawlerPB.EntityEntityTdName ""

getCrawlerTypeAsText :: EntityType -> Text
getCrawlerTypeAsText entity' = case entity' of
  CrawlerPB.EntityEntityProjectName _ -> "project"
  CrawlerPB.EntityEntityOrganizationName _ -> "organization"
  CrawlerPB.EntityEntityTdName _ -> "taskdata"

ensureCrawlerMetadata :: Text -> QueryM UTCTime -> Entity -> QueryM ()
ensureCrawlerMetadata crawlerName getDate entity = do
  index <- getIndexName
  exists <- BH.documentExists index getId
  unless exists $ do
    lastUpdatedDate <- getDate
    withRefresh $ BH.indexDocument index BH.defaultIndexDocumentSettings (cm lastUpdatedDate) getId
  where
    cm lastUpdatedDate =
      ECrawlerMetadata
        { ecmCrawlerMetadata =
            ECrawlerMetadataObject
              (from crawlerName)
              (from entity)
              (from $ getEntityName entity)
              lastUpdatedDate
        }
    getId = getCrawlerMetadataDocId crawlerName (from entity) (getEntityName entity)

getMostRecentUpdatedChange :: QueryMonad m => Text -> m [EChange]
getMostRecentUpdatedChange fullname = do
  withFilter [mkTerm "repository_fullname" fullname] $ Q.changes (Just order) 1
  where
    order =
      SearchPB.Order
        { orderField = "updated_at",
          orderDirection = PT.Enumerated $ Right SearchPB.Order_DirectionDESC
        }

-- | Maybe return the most recent updatedAt date for a repository full name
getLastUpdatedDate :: QueryMonad m => Text -> m (Maybe UTCTime)
getLastUpdatedDate fullname = do
  recents <- getMostRecentUpdatedChange fullname
  pure $ case recents of
    [] -> Nothing
    (c : _) -> Just $ c & echangeUpdatedAt

setLastUpdated :: Text -> UTCTime -> Entity -> QueryM ()
setLastUpdated crawlerName lastUpdatedDate entity = do
  index <- getIndexName
  withRefresh $ BH.updateDocument index BH.defaultIndexDocumentSettings cm getId
  where
    getId = getCrawlerMetadataDocId crawlerName (from entity) (getEntityName entity)
    cm =
      ECrawlerMetadata
        { ecmCrawlerMetadata =
            ECrawlerMetadataObject
              (from crawlerName)
              (from entity)
              (from $ getEntityName entity)
              lastUpdatedDate
        }

initCrawlerEntities :: [Entity] -> Config.Crawler -> QueryM ()
initCrawlerEntities entities worker = traverse_ run entities
  where
    run :: Entity -> QueryM ()
    run entity = do
      let updated_since =
            fromMaybe defaultUpdatedSince <$> case entity of
              Project name -> getLastUpdatedDate $ fromMaybe "" (Config.getPrefix worker) <> name
              _ -> pure Nothing
      ensureCrawlerMetadata (getWorkerName worker) updated_since entity
    defaultUpdatedSince = getWorkerUpdatedSince worker

getProjectEntityFromCrawler :: Config.Crawler -> [Entity]
getProjectEntityFromCrawler worker = Project <$> Config.getCrawlerProject worker

getOrganizationEntityFromCrawler :: Config.Crawler -> [Entity]
getOrganizationEntityFromCrawler worker = Organization <$> Config.getCrawlerOrganization worker

getTaskDataEntityFromCrawler :: Config.Crawler -> [Entity]
getTaskDataEntityFromCrawler worker = TaskDataEntity <$> Config.getCrawlerTaskData worker

initCrawlerMetadata :: Config.Crawler -> QueryM ()
initCrawlerMetadata crawler =
  initCrawlerEntities
    ( getProjectEntityFromCrawler crawler
        <> getOrganizationEntityFromCrawler crawler
        <> getTaskDataEntityFromCrawler crawler
    )
    crawler

-- Author cache functions
-------------------------

toCachedAuthorValue :: Text -> Value
toCachedAuthorValue muid = toJSON $ CachedAuthor ECachedAuthor (from muid)

-- | Wipe then fill the author cache
-- The CachedAuthor list is built from all uniq Author in the EL index
populateAuthorCache :: QueryM Int
populateAuthorCache = do
  indexName <- getIndexName
  -- First wipe the cache
  void $
    withFilter [Q.documentType ECachedAuthor] $
      Q.scanSearchId
        & ( Streaming.map (BulkDelete indexName)
              >>> bulkStream
          )
  -- Second populate the cache
  Q.getAllAuthorsMuid
    & ( Streaming.map (mkECachedAuthorBulkInsert indexName)
          >>> bulkStream
      )
  where
    mkECachedAuthorBulkInsert :: BH.IndexName -> Text -> BulkOperation
    mkECachedAuthorBulkInsert indexName muid =
      BulkIndex indexName (getBHDocID muid) $ toCachedAuthorValue muid

-- | This function extacts authors from events and adds them to the author cache
addCachedAuthors :: [EChangeEvent] -> QueryM ()
addCachedAuthors events = do
  indexName <- getIndexName
  let muids = from . authorMuid <$> mapMaybe echangeeventAuthor events
      bulkOps = mkECachedAuthorBulkUpsert indexName <$> muids
  void $ BH.bulk $ fromList bulkOps
  void $ BH.refreshIndex indexName
  where
    mkECachedAuthorBulkUpsert indexName muid =
      BulkUpsert indexName (getBHDocID muid) (BH.UpsertDoc $ toCachedAuthorValue muid) []

-- | This function returns the author cache contents
getAuthorCache :: QueryM [CachedAuthor]
getAuthorCache =
  withFilter
    [Q.documentType ECachedAuthor]
    Q.scanSearchSimple

-- | This function returns matched author muid(s) based on the match query
searchAuthorCache :: Text -> QueryM [Text]
searchAuthorCache matchQuery = do
  indexName <- getIndexName
  ret <- runSearch indexName
  pure $ mapMaybe trans ret
  where
    runSearch :: (MonadBH m, MonadThrow m) => BH.IndexName -> m [BH.Hit CachedAuthor]
    runSearch index = BH.scanSearch index search
    search = BH.mkSearch (Just query) Nothing
    query =
      BH.QueryMatchQuery . BH.mkMatchQuery (BH.FieldName "cached_author_muid") $
        BH.QueryString matchQuery
    trans :: BH.Hit CachedAuthor -> Maybe Text
    trans BH.Hit {..} = case hitSource of
      Just CachedAuthor {..} -> Just . from $ caCachedAuthorMuid
      _ -> Nothing
