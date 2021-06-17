{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Index where

import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value,
    object,
  )
import Data.Time
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as T
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents
import qualified Monocle.Backend.Queries as Q
import Monocle.Change
import qualified Monocle.Crawler as CrawlerPB
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as NHTS
import Relude

type MServerName = Text

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

mkEnv :: MonadIO m => MServerName -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

tenantIndexName :: Config.Index -> BH.IndexName
tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> index

ensureIndex :: MonadIO m => BH.BHEnv -> Config.Index -> m BH.IndexName
ensureIndex bhEnv config@Config.Index {..} = do
  liftIO . BH.runBH bhEnv $ do
    _respCI <- BH.createIndex indexSettings indexName
    -- print respCI
    _respPM <- BH.putMapping indexName ChangesIndexMapping
    -- print respPM
    True <- BH.indexExists indexName
    pure ()
  liftIO $ traverse_ (initCrawlerLastUpdatedFromWorkerConfig bhEnv indexName) (fromMaybe [] crawlers)
  pure indexName
  where
    indexName = tenantIndexName config
    indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0)

createChangesIndex :: MServerName -> Config.Index -> IO (BH.BHEnv, BH.IndexName)
createChangesIndex serverUrl config = do
  bhEnv <- mkEnv serverUrl
  indexName <- ensureIndex bhEnv config
  pure (bhEnv, indexName)

-- intC = fromInteger . toInteger

toAuthor :: Maybe Monocle.Change.Ident -> Monocle.Backend.Documents.Author
toAuthor (Just Monocle.Change.Ident {..}) =
  Monocle.Backend.Documents.Author
    { authorMuid = identMuid,
      authorUid = identUid
    }
toAuthor Nothing = error "Ident field is mandatory"

toELKChangeEvent :: ChangeEvent -> ELKChangeEvent
toELKChangeEvent ChangeEvent {..} =
  ELKChangeEvent
    { elkchangeeventId = changeEventId,
      elkchangeeventNumber = fromIntegral changeEventNumber,
      elkchangeeventType = getEventType changeEventType,
      elkchangeeventChangeId = changeEventChangeId,
      elkchangeeventUrl = changeEventUrl,
      elkchangeeventChangedFiles = Just $ map changedFilePathPath $ toList changeEventChangedFiles,
      elkchangeeventRepositoryPrefix = changeEventRepositoryPrefix,
      elkchangeeventRepositoryFullname = changeEventRepositoryFullname,
      elkchangeeventRepositoryShortname = changeEventRepositoryShortname,
      elkchangeeventAuthor = toAuthor changeEventAuthor,
      elkchangeeventOnAuthor = toAuthor changeEventOnAuthor,
      elkchangeeventBranch = changeEventBranch,
      elkchangeeventOnCreatedAt = T.toUTCTime $ fromMaybe (error "changeEventOnCreatedAt field is mandatory") changeEventOnCreatedAt,
      elkchangeeventApproval = case changeEventType of
        Just (ChangeEventTypeChangeReviewed (ChangeReviewedEvent approval)) -> Just $ toList approval
        _ -> Nothing
    }
  where
    getEventType :: Maybe ChangeEventType -> LText
    getEventType eventTypeM = case eventTypeM of
      Just eventType -> case eventType of
        ChangeEventTypeChangeCreated ChangeCreatedEvent -> "ChangeCreatedEvent"
        ChangeEventTypeChangeCommented ChangeCommentedEvent -> "ChangeCommentedEvent"
        ChangeEventTypeChangeAbandoned ChangeAbandonedEvent -> "ChangeAbandonedEvent"
        ChangeEventTypeChangeReviewed (ChangeReviewedEvent _) -> "ChangeReviewedEvent"
        ChangeEventTypeChangeForcePushed ChangeForcePushedEvent -> "ChangeForcePushedEvent"
        ChangeEventTypeChangePushed ChangePushedEvent -> "ChangePushedEvent"
        ChangeEventTypeChangeMerged ChangeMergedEvent -> "ChangeMergedEvent"
      Nothing -> error "changeEventType field is mandatory"

toELKChange :: Change -> ELKChange
toELKChange Change {..} =
  ELKChange
    { elkchangeId = changeId,
      elkchangeType = "Change",
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
      elkchangeState = changeState,
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

indexDocs :: BH.BHEnv -> BH.IndexName -> [(Value, BH.DocId)] -> IO ()
indexDocs bhEnv index docs = BH.runBH bhEnv $ do
  let stream = V.fromList $ fmap toBulkIndex docs
  _ <- BH.bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- BH.refreshIndex index
  pure ()
  where
    -- BulkIndex operation: Create the document, replacing it if it already exists.
    toBulkIndex (doc, docId) = BH.BulkIndex index docId doc

getChangeDocId :: ELKChange -> BH.DocId
getChangeDocId change = BH.DocId . toText $ elkchangeId change

indexChanges :: BH.BHEnv -> BH.IndexName -> [ELKChange] -> IO ()
indexChanges bhEnv index changes = indexDocs bhEnv index $ fmap (toDoc . ensureType) changes
  where
    toDoc change = (toJSON change, getChangeDocId change)
    ensureType change = change {elkchangeType = "Change"}

getEventDocId :: ELKChangeEvent -> BH.DocId
getEventDocId event = BH.DocId . toStrict $ elkchangeeventChangeId event

indexEvents :: BH.BHEnv -> BH.IndexName -> [ELKChangeEvent] -> IO ()
indexEvents bhEnv index events = indexDocs bhEnv index (fmap toDoc events)
  where
    toDoc ev = (toJSON ev, BH.DocId . toStrict $ elkchangeeventChangeId ev)

statusCheck :: (Int -> c) -> HTTP.Response body -> c
statusCheck prd = prd . NHTS.statusCode . HTTP.responseStatus

isNotFound :: BH.Reply -> Bool
isNotFound = statusCheck (== 404)

checkDocExists :: BH.BHEnv -> BH.IndexName -> BH.DocId -> IO Bool
checkDocExists bhEnv index docId = do
  BH.runBH bhEnv $ do
    BH.documentExists index docId

getDocument :: (FromJSON a) => BH.BHEnv -> BH.IndexName -> BH.DocId -> IO (Maybe a)
getDocument bhEnv index dId = do
  BH.runBH bhEnv $ do
    resp <- BH.getDocument index dId
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

type CrawlerMetadataDocId = BH.DocId

getCrawlerMetadataDocId :: Text -> Text -> Text -> CrawlerMetadataDocId
getCrawlerMetadataDocId crawlerName crawlerType crawlerTypeValue =
  BH.DocId . toText $
    intercalate
      "-"
      [ toString crawlerName,
        toString crawlerType,
        toString crawlerTypeValue
      ]

getCrawlerMetadata :: BH.BHEnv -> BH.IndexName -> CrawlerMetadataDocId -> IO (Maybe ELKCrawlerMetadata)
getCrawlerMetadata = getDocument

getLastUpdatedFromConfig :: UTCTime
getLastUpdatedFromConfig = parseTimeOrError False defaultTimeLocale "%F" "2021-01-01"

data Entity = Project {getName :: Text} | Organization {getName :: Text}

type EntityType = CrawlerPB.CommitInfoRequest_EntityType

getWorkerName :: Config.Crawler -> Text
getWorkerName Config.Crawler {..} = name

getLastUpdated :: BH.BHEnv -> BH.IndexName -> Config.Crawler -> EntityType -> IO (Text, UTCTime)
getLastUpdated bhEnv index crawler entity = do
  BH.runBH bhEnv $ do
    resp <- fmap BH.hitSource <$> (Q.simpleSearch index search :: BH.BH IO [BH.Hit ELKCrawlerMetadata])
    case catMaybes resp of
      [] -> error "Unsupported"
      (x : _) -> pure $ getRespFromMetadata x
  where
    search =
      (BH.mkSearch (Just query) Nothing)
        { BH.size = BH.Size 1,
          BH.sortBody = Just [BH.DefaultSortSpec bhSort]
        }

    bhSort = BH.DefaultSort (BH.FieldName "crawler_metadata.last_commit_at") BH.Ascending Nothing Nothing Nothing Nothing
    query =
      Q.mkAnd
        [ BH.TermQuery (BH.Term "crawler_metadata.crawler_name" (getWorkerName crawler)) Nothing,
          BH.TermQuery (BH.Term "crawler_metadata.crawler_type" (crawlerType entity)) Nothing
        ]
    crawlerType :: EntityType -> Text
    crawlerType entity' = case entity' of
      CrawlerPB.CommitInfoRequest_EntityTypeProject -> "project"
      _ -> error "Unsupported Entity"
    getRespFromMetadata (ELKCrawlerMetadata ELKCrawlerMetadataObject {..}) =
      (toStrict elkcmCrawlerTypeValue, elkcmLastCommitAt)

setOrUpdateLastUpdated :: Bool -> BH.BHEnv -> BH.IndexName -> Text -> UTCTime -> Entity -> IO ()
setOrUpdateLastUpdated doNotUpdate bhEnv index crawlerName lastUpdatedDate entity = do
  BH.runBH bhEnv $ do
    exists <- BH.documentExists index id'
    when ((exists && not doNotUpdate) || not exists) $ do
      resp <-
        if exists
          then BH.updateDocument index BH.defaultIndexDocumentSettings cm id'
          else BH.indexDocument index BH.defaultIndexDocumentSettings cm id'
      _ <- BH.refreshIndex index
      if BH.isSuccess resp then pure () else error "Unable to set Crawler Metadata"
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
    crawlerType entity' = case entity' of
      Project _ -> "project"
      _ -> error "Unsupported Entity"

setLastUpdated :: BH.BHEnv -> BH.IndexName -> Text -> UTCTime -> Entity -> IO ()
setLastUpdated = setOrUpdateLastUpdated False

initCrawlerLastUpdatedFromWorkerConfig :: BH.BHEnv -> BH.IndexName -> Config.Crawler -> IO ()
initCrawlerLastUpdatedFromWorkerConfig bhEnv index worker = traverse_ run entities
  where
    run = setOrUpdateLastUpdated True bhEnv index (getWorkerName worker) (getWorkerUpdatedSince worker)
    entities = Project <$> Config.getCrawlerProject worker
    getWorkerUpdatedSince Config.Crawler {..} =
      fromMaybe (error "nop") (readMaybe (toString update_since) :: Maybe UTCTime)
