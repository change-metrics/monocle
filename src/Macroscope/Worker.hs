-- TEMP, to remove when org and task data are migrated to this new system
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker interface.
module Macroscope.Worker (
  runStream,
  DocumentStream (..),
) where

import Data.Vector qualified as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille
import Monocle.Entity
import Monocle.Prelude
import Monocle.Protob.Change (Change, ChangeEvent)
import Monocle.Protob.Crawler as CrawlerPB hiding (Entity)
import Monocle.Protob.Search (TaskData)
import Proto3.Suite (Enumerated (Enumerated))
import Streaming qualified as S
import Streaming.Prelude qualified as S

import Effectful qualified as E
import Effectful.Reader.Static qualified as E
import Monocle.Effects

-- | A crawler is defined as a DocumentStream:
data DocumentStream es
  = -- | Fetch projects for a organization name
    Projects (Text -> LentilleStream es Project)
  | -- | Fetch recent changes from a project
    Changes (UTCTime -> Text -> LentilleStream es (Change, [ChangeEvent]))
  | -- | Fetch recent task data
    TaskDatas (UTCTime -> Text -> LentilleStream es TaskData)

-- | Get the entity type managed by a given stream
streamEntity :: DocumentStream es -> CrawlerPB.EntityType
streamEntity = \case
  Projects _ -> EntityTypeENTITY_TYPE_ORGANIZATION
  Changes _ -> EntityTypeENTITY_TYPE_PROJECT
  TaskDatas _ -> EntityTypeENTITY_TYPE_TASK_DATA

-- | Get a text representation of a stream type
streamName :: DocumentStream m -> Text
streamName = \case
  Projects _ -> "Projects"
  Changes _ -> "Changes"
  TaskDatas _ -> "TaskDatas"

isTDStream :: DocumentStream m -> Bool
isTDStream = \case
  TaskDatas _ -> True
  _anyOtherStream -> False

-------------------------------------------------------------------------------
-- Adapter between protobuf api and crawler stream
-------------------------------------------------------------------------------
type ApiKey = LText

type IndexName = LText

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
  forall es.
  -- | Funtion to log about the processing
  (Int -> Eff es ()) ->
  -- | Function to post on the Monocle API
  ([DocumentType] -> Eff es AddDocResponse) ->
  -- | The stream of documents to read
  Stream (Of DocumentType) (Eff es) () ->
  -- | The processing results
  Eff es [ProcessResult]
process logFunc postFunc =
  S.toList_
    . S.mapM processBatch
    . S.mapped S.toList
    . S.chunksOf 500
 where
  processBatch :: [DocumentType] -> Eff es ProcessResult
  processBatch docs = do
    logFunc (length docs)
    resp <- postFunc docs
    pure $ case resp of
      AddDocResponse Nothing -> AddOk
      AddDocResponse (Just err) -> AddError (show err)

-- | Run is the main function used by macroscope
runStream ::
  forall es.
  [LoggerEffect, Retry, PrometheusEffect, E.Reader CrawlerEnv, MonoClientEffect, TimeEffect] :>> es =>
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream (Error LentilleError : es) ->
  Eff es ()
runStream apiKey indexName crawlerName documentStream = do
  startTime <- mGetCurrentTime
  withContext ("index" .= indexName <> "crawler" .= crawlerName <> "stream" .= streamName documentStream) do
    runStream' startTime apiKey indexName crawlerName documentStream

runStream' ::
  forall es.
  [E.Reader CrawlerEnv, LoggerEffect, Retry, PrometheusEffect, MonoClientEffect] :>> es =>
  UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream (Error LentilleError : es) ->
  Eff es ()
runStream' startTime apiKey indexName (CrawlerName crawlerName) documentStream = drainEntities (0 :: Word32)
 where
  drainEntities :: Word32 -> Eff es ()
  drainEntities offset =
    unlessStopped do
      -- TODO: simplify stream error handling by using a `Stream (Of (Either Error a))`
      -- to avoid using exceptions.
      res <- runErrorNoCallStack (safeDrainEntities offset)
      case res of
        Right () -> pure ()
        Left e -> handleStreamError offset e

  safeDrainEntities :: Word32 -> Eff (Error LentilleError : es) ()
  safeDrainEntities offset = do
    logInfo "Looking for oldest entity" ["offset" .= offset]

    -- Query the monocle api for the oldest entity to be updated.
    oldestEntityM <- getStreamOldestEntity indexName (from crawlerName) (streamEntity documentStream) offset
    case oldestEntityM of
      Nothing -> logInfo_ "Unable to find entity to update"
      Just (oldestAge, entity)
        | -- add a 1 second delta to avoid Hysteresis
          addUTCTime 1 oldestAge >= startTime ->
            logInfo "Crawling entities completed" ["entity" .= entity, "age" .= oldestAge]
        | otherwise -> do
            let processLogFunc c = logInfo "Posting documents" ["count" .= c]
            logInfo "Processing" ["entity" .= entity, "age" .= oldestAge]

            -- Run the document stream for that entity
            postResult <-
              process
                processLogFunc
                (httpRetry "api/commit/add" . addDoc entity)
                (getStream oldestAge entity)
            case foldr collectPostFailure [] postResult of
              [] -> do
                -- Post the commit date
                res <- httpRetry "api/commit" $ commitTimestamp entity
                case res of
                  Nothing -> do
                    logInfo_ "Continuing on next entity"
                    -- Note: to call the error handler (drainEntities Eff es) from (Error : es) we need
                    -- to Lift it with the error effect using 'raise'.
                    E.raise (drainEntities offset)
                  Just (err :: Text) -> do
                    logWarn "Commit date failed" ["err" .= err]
              xs -> logWarn "Postt documents tailed" ["errors" .= xs]

  handleStreamError :: Word32 -> LentilleError -> Eff es ()
  handleStreamError offset err = do
    logWarn "Error occured when consuming the document stream" ["err" .= show @Text err]
    -- TODO: log a structured error on filesystem or audit index in elastic
    unless (isTDStream documentStream) $ drainEntities (offset + 1)

  collectPostFailure :: ProcessResult -> [Text] -> [Text]
  collectPostFailure res acc = case res of
    AddOk -> acc
    AddError err -> err : acc

  -- Adapt the document stream to intermediate representation
  getStream oldestAge entity = case documentStream of
    Changes s ->
      let project = extractEntityValue _Project
       in S.map DTChanges (s oldestAge project)
    Projects s ->
      let organization = extractEntityValue _Organization
       in S.map DTProject (s organization)
    TaskDatas s ->
      let td = extractEntityValue _TaskDataEntity
       in S.map DTTaskData (s oldestAge td)
   where
    extractEntityValue prism =
      fromMaybe (error $ "Entity is not the right shape: " <> show entity) $
        preview prism entity

  addDoc :: Entity -> [DocumentType] -> Eff (Error LentilleError : es) AddDocResponse
  addDoc entity xs = mCrawlerAddDoc $ mkRequest entity xs

  -- 'mkRequest' creates the 'AddDocRequests' for a given oldest entity and a list of documenttype
  -- this is used by the processBatch function.
  mkRequest :: Entity -> [DocumentType] -> AddDocRequest
  mkRequest entity xs =
    let addDocRequestIndex = indexName
        addDocRequestCrawler = from crawlerName
        addDocRequestApikey = apiKey
        addDocRequestEntity = Just (from entity)
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
  commitTimestamp entity = do
    commitResp <-
      mCrawlerCommit
        ( CommitRequest
            indexName
            (from crawlerName)
            apiKey
            (Just $ from entity)
            (Just $ Timestamp.fromUTCTime startTime)
        )
    pure $ case commitResp of
      (CommitResponse (Just (CommitResponseResultTimestamp _))) -> Nothing
      (CommitResponse (Just (CommitResponseResultError err))) -> Just (show err)
      _ -> Just "Empty commit response"

-- | Adapt the API response
getStreamOldestEntity ::
  [PrometheusEffect, LoggerEffect, Retry, MonoClientEffect] :>> es =>
  LText ->
  LText ->
  CrawlerPB.EntityType ->
  Word32 ->
  Eff es (Maybe (UTCTime, Monocle.Entity.Entity))
getStreamOldestEntity indexName crawlerName entityType offset = do
  let commitRequest = CommitInfoRequest indexName crawlerName (toPBEnum entityType) offset
  resp <- httpRetry "api/commit/info" $ mCrawlerCommitInfo commitRequest
  case resp of
    CommitInfoResponse
      ( Just
          ( CommitInfoResponseResultEntity
              (CommitInfoResponse_OldestEntity (Just entity) (Just ts))
            )
        ) ->
        pure $ Just (from ts, from entity)
    CommitInfoResponse
      ( Just
          ( CommitInfoResponseResultError
              (Enumerated (Right CommitInfoErrorCommitGetNoEntity))
            )
        ) -> pure Nothing
    _ -> error $ "Could not get initial timestamp: " <> show resp
