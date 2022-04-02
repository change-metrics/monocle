-- TEMP, to remove when org and task data are migrated to this new system
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker interface.
module Macroscope.Worker
  ( runStream,
    DocumentStream (..),
  )
where

import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille
import Monocle.Backend.Index (entityRequestOrganization, entityRequestProject, entityRequestTaskData)
import Monocle.Change (Change, ChangeEvent)
import Monocle.Crawler
import Monocle.Prelude
import Monocle.Search (TaskData)
import Proto3.Suite (Enumerated (Enumerated))
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | A crawler is defined as a DocumentStream:
data DocumentStream m
  = -- | Fetch projects for a organization name
    Projects (Text -> LentilleStream m Project)
  | -- | Fetch recent changes from a project
    Changes (UTCTime -> Text -> LentilleStream m (Change, [ChangeEvent]))
  | -- | Fetch recent task data
    TaskDatas (UTCTime -> Text -> LentilleStream m TaskData)

isTDStream :: DocumentStream m -> Bool
isTDStream = \case
  TaskDatas _ -> True
  _anyOtherStream -> False

-------------------------------------------------------------------------------
-- Adapter between protobuf api and crawler stream
-------------------------------------------------------------------------------

type ApiKey = LText

type IndexName = LText

type CrawlerName = LText

-- | 'getDate' gets the updated time of an 'OldestEntity'
getDate :: OldestEntity -> UTCTime
getDate oe =
  toUTCTime
    . fromMaybe (error "missing TS")
    $ commitInfoResponse_OldestEntityLastCommitAt oe

-- | 'getTaskdata' gets the updated time and project name from an 'OldestEntity'
getTaskdata :: OldestEntity -> (UTCTime, Text)
getTaskdata oe = (getDate oe, toStrict taskdata)
  where
    taskdata =
      case commitInfoResponse_OldestEntityEntity oe of
        Just (Entity (Just (EntityEntityTdName name))) -> name
        _ -> error $ "Not a task data: " <> show oe

-- | 'getProject' gets the updated time and project name from an 'OldestEntity'
getProject :: OldestEntity -> (UTCTime, Text)
getProject oe = (getDate oe, toStrict project)
  where
    project =
      case commitInfoResponse_OldestEntityEntity oe of
        Just (Entity (Just (EntityEntityProjectName name))) -> name
        _ -> error $ "Not a project: " <> show oe

-- | 'getOrganization' gets the updated time and organization name from an 'OldestEntity'
getOrganization :: OldestEntity -> (UTCTime, Text)
getOrganization oe = (getDate oe, toStrict organization)
  where
    organization =
      case commitInfoResponse_OldestEntityEntity oe of
        Just (Entity (Just (EntityEntityOrganizationName name))) -> name
        _ -> error $ "Not an organization: " <> show oe

-------------------------------------------------------------------------------
-- Worker implementation
-------------------------------------------------------------------------------

-- | The crawler stream is (locally) converted to a Stream (Of DocumentType)
-- This intermediary representation enables generic processing with 'processBatch'
data DocumentType
  = DTProject Project
  | DTChanges (Change, [ChangeEvent])
  | DTTaskData TaskData

data ProcessResult = AddOk | AddError Text deriving stock (Show)

type OldestEntity = CommitInfoResponse_OldestEntity

-- | 'process' read the stream of document and post to the monocle API
process ::
  forall m.
  MonadLog m =>
  -- | Funtion to log about the processing
  (Int -> Log) ->
  -- | Function to post on the Monocle API
  ([DocumentType] -> m AddDocResponse) ->
  -- | The stream of documents to read
  Stream (Of DocumentType) m () ->
  -- | The processing results
  m [ProcessResult]
process logFunc postFunc =
  S.toList_
    . S.mapM processBatch
    . S.mapped S.toList
    . S.chunksOf 500
  where
    processBatch :: [DocumentType] -> m ProcessResult
    processBatch docs = do
      mLog $ logFunc (length docs)
      resp <- postFunc docs
      pure $ case resp of
        AddDocResponse Nothing -> AddOk
        AddDocResponse (Just err) -> AddError (show err)

type MonadCrawlerE m = (MonadCrawler m, MonadReader CrawlerEnv m)

-- | Run is the main function used by macroscope
runStream ::
  (MonadCatch m, MonadLog m, MonadRetry m, MonadMonitor m, MonadCrawlerE m) =>
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream m ->
  m ()
runStream apiKey indexName crawlerName documentStream = do
  -- It is important to get the commit date before starting the process to not miss
  -- document updated when we start
  startTime <- mGetCurrentTime
  runStream' startTime apiKey indexName crawlerName documentStream

runStream' ::
  (MonadCatch m, MonadLog m, MonadRetry m, MonadMonitor m, MonadCrawlerE m) =>
  UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream m ->
  m ()
runStream' startTime apiKey indexName crawlerName documentStream = drainEntities (0 :: Word32)
  where
    lc = LogCrawlerContext (toText indexName) (toText crawlerName) Nothing
    wLog event = mLog $ Log Macroscope event
    drainEntities offset =
      unlessStopped $
        safeDrainEntities offset `catch` handleStreamError offset

    safeDrainEntities offset = do
      wLog $ LogMacroRequestOldestEntity lc (streamType documentStream)
      monocleBaseUrl <- getClientBaseUrl
      let ident = "api-client"

      -- Query the monocle api for the oldest entity to be updated.
      entityM <- httpRetry (ident, monocleBaseUrl, "internal") $ getOldestEntity offset
      case entityM of
        Nothing -> wLog $ LogMacroNoOldestEnity lc
        Just entity -> do
          let (eType, eName) = oldestEntityEntityToText entity
              processLogFunc c = Log Macroscope $ LogMacroPostData lc eName c
          wLog $ LogMacroGotOldestEntity lc (eType, eName) (oldestEntityDate entity)

          -- add a 1 second delta to avoid Hysteresis
          if addUTCTime 1 (oldestEntityDate entity) >= startTime
            then wLog $ LogMacroEnded lc
            else do
              -- Run the document stream for that entity
              postResult <-
                process
                  processLogFunc
                  (httpRetry (ident, monocleBaseUrl, "internal") . addDoc entity)
                  (getStream entity)
              case foldr collectPostFailure [] postResult of
                [] -> do
                  -- Post the commit date
                  res <- httpRetry (ident, monocleBaseUrl, "internal") $ commitTimestamp entity
                  if not res
                    then wLog $ LogMacroCommitFailed lc
                    else do
                      wLog $ LogMacroContinue lc
                      drainEntities offset
                xs -> wLog $ LogMacroPostDataFailed lc xs

    handleStreamError offset err = do
      wLog $ LogMacroStreamError lc (show (err :: LentilleError))
      -- TODO: log a structured error on filesystem or audit index in elastic
      unless (isTDStream documentStream) $ drainEntities (offset + 1)

    collectPostFailure :: ProcessResult -> [Text] -> [Text]
    collectPostFailure res acc = case res of
      AddOk -> acc
      AddError err -> err : acc

    oldestEntityDate oe = case oe of
      CommitInfoResponse_OldestEntity _ (Just tc) -> Timestamp.toUTCTime tc
      _ -> error "Timestamp missing"

    oldestEntityEntityToText :: CommitInfoResponse_OldestEntity -> (Text, Text)
    oldestEntityEntityToText oe = case oe of
      CommitInfoResponse_OldestEntity (Just entity) _ -> eToText entity
      _ -> error "Entity missing"
      where
        eToText (Entity e) = case e of
          Just (EntityEntityOrganizationName v) -> ("Organization", toText v)
          Just (EntityEntityProjectName v) -> ("Project", toText v)
          Just (EntityEntityTdName v) -> ("TaskData", toText v)
          _ -> error "EntityEntity missing"

    -- Adapt the document stream to intermediate representation
    getStream oldestEntity = case documentStream of
      Changes s ->
        let (untilDate, project) = getProject oldestEntity
         in S.map DTChanges (s untilDate project)
      Projects s ->
        let (_, organization) = getOrganization oldestEntity
         in S.map DTProject (s organization)
      TaskDatas s ->
        let (untilDate, td) = getTaskdata oldestEntity
         in S.map DTTaskData (s untilDate td)

    -- Get a stream representation of a stream type
    streamType = \case
      Projects _ -> "Projects"
      Changes _ -> "Changes"
      TaskDatas _ -> "TaskDatas"

    getOldestEntity :: MonadCrawlerE m => Word32 -> m (Maybe CommitInfoResponse_OldestEntity)
    getOldestEntity offset = do
      client <- asks crawlerClient
      resp <-
        mCrawlerCommitInfo
          client
          ( CommitInfoRequest
              indexName
              crawlerName
              (Just $ Entity $ Just entityType)
              offset
          )
      case resp of
        CommitInfoResponse (Just (CommitInfoResponseResultEntity entity)) -> pure $ Just entity
        CommitInfoResponse (Just (CommitInfoResponseResultError (Enumerated (Right CommitInfoErrorCommitGetNoEntity)))) -> pure Nothing
        _ -> error $ "Could not get initial timestamp: " <> show resp
      where
        -- The type of the oldest entity for a given document stream
        entityType = case documentStream of
          Projects _ -> entityRequestOrganization
          Changes _ -> entityRequestProject
          TaskDatas _ -> entityRequestTaskData

    addDoc :: MonadCrawlerE m => OldestEntity -> [DocumentType] -> m AddDocResponse
    addDoc entity xs = do
      client <- asks crawlerClient
      mCrawlerAddDoc client $ mkRequest entity xs
    -- 'mkRequest' creates the 'AddDocRequests' for a given oldest entity and a list of documenttype
    -- this is used by the processBatch function.
    mkRequest :: OldestEntity -> [DocumentType] -> AddDocRequest
    mkRequest oe xs =
      let addDocRequestIndex = indexName
          addDocRequestCrawler = crawlerName
          addDocRequestApikey = apiKey
          addDocRequestEntity = commitInfoResponse_OldestEntityEntity oe
          addDocRequestChanges = V.fromList $ mapMaybe getChanges xs
          addDocRequestEvents = V.fromList $ concat $ mapMaybe getEvents xs
          addDocRequestProjects = V.fromList $ mapMaybe getProject' xs
          addDocRequestTaskDatas = V.fromList $ mapMaybe getTD xs
       in AddDocRequest {..}
      where
        getEvents = \case
          DTChanges (_, events) -> Just events
          _ -> Nothing
        getChanges = \case
          DTChanges (change, _) -> Just change
          _ -> Nothing
        getProject' = \case
          DTProject p -> Just p
          _ -> Nothing
        getTD = \case
          DTTaskData td -> Just td
          _ -> Nothing

    -- 'commitTimestamp' post the commit date.
    commitTimestamp oe = do
      client <- asks crawlerClient
      commitResp <-
        mCrawlerCommit
          client
          ( CommitRequest
              indexName
              crawlerName
              apiKey
              (commitInfoResponse_OldestEntityEntity oe)
              (Just $ Timestamp.fromUTCTime startTime)
          )
      case commitResp of
        (CommitResponse (Just (CommitResponseResultTimestamp _))) -> pure True
        (CommitResponse (Just (CommitResponseResultError err))) -> do
          logRaw ("Commit failed: " <> show err)
          pure False
        _ -> do
          logRaw "Empty commit response"
          pure False
