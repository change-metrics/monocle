-- | Utility function to clean up and maintain the backend
module Monocle.Backend.Janitor
  ( removeTDCrawlerData,
    wipeCrawlerData,
    updateIdentsOnEvents,
    updateIdentsOnChanges,
    updateIdentsOnWorkspace,
  )
where

import Data.Aeson (genericParseJSON, genericToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Text as T
import qualified Database.Bloodhound as BH
import Monocle.Api.Config (getIdentByAlias)
import qualified Monocle.Api.Config as C
import Monocle.Backend.Documents as D
import qualified Monocle.Backend.Index as I
import Monocle.Backend.Queries as Q
import Monocle.Env
import Monocle.Prelude
import qualified Streaming.Prelude as Streaming

updateAuthor :: C.Index -> D.Author -> D.Author
updateAuthor index author@D.Author {..} = case getIdent of
  Just ident -> D.Author ident authorUid
  Nothing
    | newMuid /= from authorMuid -> D.Author (from newMuid) authorUid
    | otherwise -> author
  where
    getIdent :: Maybe LText
    getIdent = from <$> getIdentByAlias index (from authorUid)
    -- Remove the host prefix
    newMuid = T.drop 1 $ T.dropWhile (/= '/') (from authorUid)

updateIdentsOnWorkspace :: QueryM ()
updateIdentsOnWorkspace = do
  workspaceName <- C.getWorkspaceName <$> asks tenant
  changesCount <- withQuery (mkQuery [Q.documentType D.EChangeDoc]) Q.countDocs
  eventsCount <- withQuery (mkQuery [Q.documentTypes $ fromList D.allEventTypes]) Q.countDocs
  print @Text $
    "Workspace "
      <> workspaceName
      <> " - Janitor will process on "
      <> show changesCount
      <> " changes and "
      <> show eventsCount
      <> " events."
  print @Text "Processing (this may take some time) ..."
  updatedChangesCount <- updateIdentsOnChanges
  print @Text $ "Updated " <> show updatedChangesCount <> " changes."
  updatedEventsCount <- updateIdentsOnEvents
  print @Text $ "Updated " <> show updatedEventsCount <> " events."

-- | Apply identities according to the configuration on Changes
-- Try this on the REPL with:
-- λ> testQueryM (defaultTenant "sf-team-workspace") $ updateIdentsOnChanges
updateIdentsOnChanges :: QueryM Int
updateIdentsOnChanges = do
  index <- asks tenant
  indexName <- getIndexName
  doUpdateIdentsOnChanges indexName (updateAuthor index)

--- Dedicated reduced data type of EChange with only Author fields
data EChangeAuthors = EChangeAuthors
  { echangeaId :: LText,
    echangeaCommits :: [Commit],
    echangeaAuthor :: Author,
    echangeaMergedBy :: Maybe Author,
    echangeaAssignees :: [Author]
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeAuthors where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeAuthors where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

doUpdateIdentsOnChanges :: BH.IndexName -> (D.Author -> D.Author) -> QueryM Int
doUpdateIdentsOnChanges indexName updateAuthor' = do
  withQuery changeQuery $
    scanChanges
      & ( Streaming.mapMaybe updateChange
            >>> Streaming.map mkEChangeBulkUpdate
            >>> I.bulkStream
        )
  where
    scanChanges :: Stream (Of EChangeAuthors) QueryM ()
    scanChanges = Q.scanSearchHit
    changeQuery = mkQuery [Q.documentType D.EChangeDoc]
    updateChange :: EChangeAuthors -> Maybe EChangeAuthors
    updateChange change@EChangeAuthors {..} =
      let updatedChange =
            change
              { echangeaAuthor = updateAuthor' echangeaAuthor,
                echangeaMergedBy = updateAuthor' <$> echangeaMergedBy,
                echangeaAssignees = updateAuthor' <$> echangeaAssignees,
                echangeaCommits = updateCommitAuthors <$> echangeaCommits
              }
       in if updatedChange == change then Nothing else Just updatedChange
      where
        updateCommitAuthors :: D.Commit -> D.Commit
        updateCommitAuthors commit@D.Commit {..} =
          commit
            { commitAuthor = updateAuthor' commitAuthor,
              commitCommitter = updateAuthor' commitCommitter
            }
    mkEChangeBulkUpdate :: EChangeAuthors -> BulkOperation
    mkEChangeBulkUpdate ec =
      BulkUpdate indexName (getChangeDocId ec) $ toJSON ec
      where
        getChangeDocId :: EChangeAuthors -> BH.DocId
        getChangeDocId change = BH.DocId . toText $ echangeaId change

-- | Apply identities according to the configuration on Events
-- Try this on the REPL with:
-- λ> testQueryM (defaultTenant "sf-team-workspace") $ updateIdentsOnEvents
updateIdentsOnEvents :: QueryM Int
updateIdentsOnEvents = do
  index <- asks tenant
  indexName <- getIndexName
  doUpdateIdentsOnEvents indexName (updateAuthor index)

--- Dedicated reduced data type of EChangeEvent with only Author fields
data EChangeEventAuthors = EChangeEventAuthors
  { echangeeventaId :: LText,
    echangeeventaAuthor :: Maybe Author,
    echangeeventaOnAuthor :: Author
  }
  deriving (Show, Eq, Generic)

instance ToJSON EChangeEventAuthors where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EChangeEventAuthors where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

doUpdateIdentsOnEvents :: BH.IndexName -> (D.Author -> D.Author) -> QueryM Int
doUpdateIdentsOnEvents indexName updateAuthor' = do
  withQuery eventQuery $
    scanEvents
      & ( Streaming.mapMaybe updateEvent
            >>> Streaming.map mkEventBulkUpdate
            >>> I.bulkStream
        )
  where
    scanEvents :: Stream (Of EChangeEventAuthors) QueryM ()
    scanEvents = Q.scanSearchHit
    eventQuery = mkQuery [Q.documentTypes $ fromList allEventTypes]

    updateEvent :: EChangeEventAuthors -> Maybe EChangeEventAuthors
    updateEvent event@EChangeEventAuthors {..} =
      let updatedEvent =
            event
              { echangeeventaAuthor = updateAuthor' <$> echangeeventaAuthor,
                echangeeventaOnAuthor = updateAuthor' echangeeventaOnAuthor
              }
       in if updatedEvent == event then Nothing else Just updatedEvent

    mkEventBulkUpdate :: EChangeEventAuthors -> BulkOperation
    mkEventBulkUpdate ev =
      BulkUpdate indexName (getEventDocId ev) $ toJSON ev
      where
        getEventDocId :: EChangeEventAuthors -> BH.DocId
        getEventDocId event = BH.DocId . toStrict $ echangeeventaId event

-- | Remove changes and events associated with a crawler name
-- Try this on the REPL with:
-- λ> testQueryM (mkConfig "zuul") $ wipeCrawlerData "custom"
wipeCrawlerData :: Text -> QueryM ()
wipeCrawlerData crawlerName = do
  -- Get index from QueryM
  config <- getIndexConfig
  -- Get crawler defintion from configuration (we need it to discover if a prefix is set)
  let crawlerM = C.lookupCrawler config crawlerName
      crawler = fromMaybe (error "Unable to find the crawler in the configuration") crawlerM
      prefixM = C.getPrefix crawler
      prefix = fromMaybe mempty prefixM
  monocleLog $ "Discovered crawler prefix: " <> (show prefixM :: Text)
  -- Get projects for this crawler from the crawler metadata objects
  projects <- getProjectsCrawler
  monocleLog $ "Discovered " <> (show $ length projects :: Text) <> " projects"
  -- For each projects delete related changes and events
  traverse_ deleteDocsByRepoName ((prefix <>) <$> projects)
  -- Finally remove crawler metadata objects
  deleteCrawlerMDs
  where
    getProjectsCrawler :: QueryM [Text]
    getProjectsCrawler = do
      projectCrawlerMDs <- withQuery sQuery Q.scanSearchSimple
      pure $ toText . getValue <$> projectCrawlerMDs
      where
        sQuery =
          mkQuery
            [ mkTerm "crawler_metadata.crawler_name" crawlerName,
              mkTerm "crawler_metadata.crawler_type" "project"
            ]
        getValue :: ECrawlerMetadata -> LText
        getValue (ECrawlerMetadata ECrawlerMetadataObject {..}) = ecmCrawlerTypeValue
    deleteDocsByRepoName :: Text -> QueryM ()
    deleteDocsByRepoName fullname = do
      monocleLog $ "Deleting " <> fullname <> " ..."
      withQuery sQuery Q.deleteDocs
      where
        sQuery =
          mkQuery
            [ mkTerm "repository_fullname" fullname,
              documentTypes . fromList $ D.allEventTypes <> [D.EChangeDoc]
            ]
    deleteCrawlerMDs :: QueryM ()
    deleteCrawlerMDs = do
      monocleLog $ "Deleting " <> crawlerName <> " crawling metadata objects ..."
      withQuery sQuery Q.deleteDocs
      where
        sQuery = mkQuery [mkTerm "crawler_metadata.crawler_name" crawlerName]

-- | Remove all the taskdata associated with a crawler name
-- Try this on the REPL with:
-- λ> testQueryM (mkConfig "zuul") $ removeTDCrawlerData "custom"
removeTDCrawlerData :: Text -> QueryM ()
removeTDCrawlerData crawlerName = do
  index <- getIndexName
  tdDeletedCount <- removeOrphanTaskDatas index
  tdChangesCount <- removeChangeTaskDatas index
  monocleLog $
    crawlerName <> ": deleted " <> show tdDeletedCount <> " td, updated " <> show tdChangesCount <> " changes"
  where
    -- Note about the structure:
    --   ($)   :: (a -> b) -> a -> b
    --   (&)   :: a -> (a -> b) -> b
    --   (>>>) :: (a -> b) -> (b -> c) -> (a -> c)
    -- Thus `f $ s & (m1 >>> m2 >>> a)` is equivalent to: `f $ a $ m2 $ m1 $ s`
    -- And this let us write the data flow from top to bottom, in particular for:
    -- - the initial `withQuery` needs to be applied first, since it operate on `QueryM`, not on `Stream`
    -- - the `scanSearchXXX` value does take argument, thus we uses `&` to insert it in the pipeline
    -- - the Streaming.map composition is more natural using `>>>` instead of `.`.
    removeChangeTaskDatas :: BH.IndexName -> QueryM Int
    removeChangeTaskDatas index =
      withQuery changeTaskDataQuery $ -- filter on changes which have a task data from that crawler
        Q.scanSearchHit -- scan the Hit (get a Stream (Of EChange))
          & ( Streaming.map removeTDFromChange -- remove the task data from the EChange
                >>> Streaming.map mkEChangeBulkUpdate -- create bulk operation
                >>> I.bulkStream -- perform the bulk operation stream
            )
      where
        changeTaskDataQuery =
          mkQuery [mkTerm "tasks_data.crawler_name" crawlerName, Q.documentType D.EChangeDoc]
        mkEChangeBulkUpdate :: EChange -> BulkOperation
        mkEChangeBulkUpdate ec =
          BulkUpdate index (I.getChangeDocId ec) (Aeson.object ["tasks_data" .= echangeTasksData ec])

        removeTDFromChange :: EChange -> EChange
        removeTDFromChange ec = ec {echangeTasksData = newTaskDatas}
          where
            newTaskDatas :: Maybe [D.ETaskData]
            newTaskDatas = case filter isNotCrawler $ fromMaybe [] (echangeTasksData ec) of
              [] -> Nothing
              x -> Just x
            isNotCrawler :: D.ETaskData -> Bool
            isNotCrawler etd
              | tdCrawlerName etd == Just crawlerName = False
              | otherwise = True

    removeOrphanTaskDatas :: BH.IndexName -> QueryM Int
    removeOrphanTaskDatas index =
      withQuery taskDataQuery $ -- filter on orphaned task data from that crawler
        Q.scanSearchId -- scan the DocId
          & ( Streaming.map mkOrphanBulkDelete -- create bulk delete operation
                >>> I.bulkStream -- perform the bulk operation stream
            )
      where
        taskDataQuery =
          mkQuery
            [ mkTerm "tasks_data.crawler_name" crawlerName,
              Q.documentType D.EOrphanTaskData
            ]
        mkOrphanBulkDelete :: DocId -> BulkOperation
        mkOrphanBulkDelete = BulkDelete index
