{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Index where

import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
  )
import Data.Time
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as T
import Monocle.Backend.Documents
import Monocle.Change
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
                        [ "last_commit_at"
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

createChangesIndex :: MServerName -> BH.IndexName -> IO (BH.BHEnv, BH.IndexName)
createChangesIndex serverUrl index = do
  let indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0)
  bhEnv <- mkEnv serverUrl
  BH.runBH bhEnv $ do
    respCI <- BH.createIndex indexSettings index
    print respCI
    respPM <- BH.putMapping index ChangesIndexMapping
    print respPM
    True <- BH.indexExists index
    pure (bhEnv, index)

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

getChangeDocId :: ELKChange -> BH.DocId
getChangeDocId change = BH.DocId . toText $ elkchangeId change

indexChanges :: BH.BHEnv -> BH.IndexName -> [ELKChange] -> IO ()
indexChanges bhEnv index changes = BH.runBH bhEnv $ do
  let stream = V.fromList (toBulkIndex . ensureType <$> changes)
  _ <- BH.bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- BH.refreshIndex index
  pure ()
  where
    -- BulkIndex operation: Create the document, replacing it if it already exists.
    toBulkIndex change =
      BH.BulkIndex index (getChangeDocId change) (toJSON change)
    ensureType change = change {elkchangeType = "Change"}

type CrawlerMetadataID = BH.DocId

getCrawlerMetadataID :: Entity -> CrawlerMetadataID
getCrawlerMetadataID entity = BH.DocId $ getEntityName entity

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

getCrawlerMetadata :: BH.BHEnv -> BH.IndexName -> CrawlerMetadataID -> IO (Maybe ELKCrawlerMetadata)
getCrawlerMetadata = getDocument

getLastUpdatedFromConfig :: UTCTime
getLastUpdatedFromConfig = parseTimeOrError False defaultTimeLocale "%F" "2021-01-01"

data Entity = Project {getName :: Text} | Organization {getName :: Text}

getEntityName :: Entity -> Text
getEntityName entity = case entity of
  Project name -> toText $ intercalate "-" ["project", toString name]
  Organization name -> toText $ intercalate "-" ["organization", toString name]

getLastUpdated :: BH.BHEnv -> BH.IndexName -> Entity -> IO UTCTime
getLastUpdated bhEnv index entity = do
  cmM <- getCrawlerMetadata bhEnv index cmID
  case cmM of
    Just cm -> pure $ elkcmLastCommitAt cm
    Nothing -> pure getLastUpdatedFromConfig
  where
    cmID = getCrawlerMetadataID entity

setLastUpdated :: BH.BHEnv -> BH.IndexName -> Entity -> UTCTime -> IO ()
setLastUpdated bhEnv index entity lastUpdatedDate = do
  BH.runBH bhEnv $ do
    exists <- BH.documentExists index id'
    resp <-
      if exists
        then do
          BH.updateDocument index BH.defaultIndexDocumentSettings cm id'
        else do
          BH.indexDocument index BH.defaultIndexDocumentSettings cm id'
    if BH.isSuccess resp then pure () else error "Unable to set Crawler Metadata"
  where
    id' = getCrawlerMetadataID entity
    cm = ELKCrawlerMetadata lastUpdatedDate
