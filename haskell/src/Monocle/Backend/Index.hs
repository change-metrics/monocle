{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Index where

import Data.Aeson
import Data.Time
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Monocle.Backend.Documents
import qualified Network.HTTP.Client as HTTP
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

getCrawlerMetadata :: BH.BHEnv -> BH.IndexName -> CrawlerMetadataID -> IO (Maybe ELKCrawlerMetadata)
getCrawlerMetadata bhEnv index cmId = do
  parsed <- BH.runBH bhEnv $ do
    resp <- BH.getDocument index cmId
    BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.EsResult ELKCrawlerMetadata))
  case parsed of
    Left _ -> error "Unable to get parse result"
    Right cm -> pure . getHit $ BH.foundResult cm
  where
    getHit :: Maybe (BH.EsResultFound ELKCrawlerMetadata) -> Maybe ELKCrawlerMetadata
    getHit (Just (BH.EsResultFound _ cm)) = Just cm
    getHit Nothing = Nothing

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
