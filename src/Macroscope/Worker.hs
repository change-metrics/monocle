{-# LANGUAGE DeriveAnyClass #-}

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
import Monocle.Protob.Issue (Issue, IssueEvent)
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
  | DTIssues (Issue, [IssueEvent])
  deriving (Generic, ToJSON)

data ProcessResult = AddOk | AddError Text deriving stock (Show)

-- | A stream error contains the first Left, and the rest of the stream
type StreamError es = (LentilleError, LentilleStream es DocumentType)

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

-- | 'runStream' is the main function used by macroscope
runStream ::
  forall es.
  (LoggerEffect :> es, Retry :> es, PrometheusEffect :> es, E.Reader CrawlerEnv :> es, MonoClientEffect :> es, TimeEffect :> es) =>
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream es ->
  Eff es ()
runStream apiKey indexName crawlerName documentStream = do
  startTime <- mGetCurrentTime
  withContext ("index" .= indexName <> "crawler" .= crawlerName <> "stream" .= streamName documentStream) do
    go startTime 0
 where
  go :: UTCTime -> Word32 -> Eff es ()
  go startTime offset =
    unlessStopped do
      res <-
        runErrorNoCallStack do
          runStreamError startTime apiKey indexName crawlerName documentStream offset
      case res of
        Right () -> pure ()
        Left (x, xs) -> do
          logWarn "Error occured when consuming the document stream" ["err" .= x]
          S.toList_ xs >>= \case
            [] -> pure ()
            rest -> logWarn "Left over documents found after error" ["items" .= rest]

          -- TODO: explains why TDStream don't support offset?
          unless (isTDStream documentStream) do
            -- Try the next entity by incrementing the offset
            go startTime (offset + 1)

-- | 'runStreamError' is the stream processor which throws an error to interupt the stream
-- when it contains a Left.
runStreamError ::
  forall es.
  (LoggerEffect :> es, Retry :> es, PrometheusEffect :> es, MonoClientEffect :> es) =>
  UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream es ->
  Word32 ->
  Eff (Error (StreamError es) : es) ()
runStreamError startTime apiKey indexName (CrawlerName crawlerName) documentStream offset = go
 where
  go = do
    logInfo "Looking for oldest entity" ["offset" .= offset]

    -- Query the monocle api for the oldest entity to be updated.
    oldestEntityM <- getStreamOldestEntity indexName (from crawlerName) (streamEntity documentStream) offset
    case oldestEntityM of
      Nothing -> logInfo_ "Unable to find entity to update"
      Just (oldestAge, entity)
        | -- add a 1 second delta to avoid Hysteresis
          addUTCTime 1 oldestAge >= startTime ->
            logInfo "Crawling entities completed" ["entity" .= entity, "age" .= oldestAge]
        | otherwise -> goStream oldestAge entity

  goStream oldestAge entity = do
    logInfo "Processing" ["entity" .= entity, "age" .= oldestAge]

    -- Run the document stream for that entity
    postResult <-
      process
        (\c -> logInfo "Posting documents" ["count" .= c])
        (httpRetry "api/commit/add" . mCrawlerAddDoc . mkRequest entity)
        (eitherStreamToError $ getStream oldestAge entity)

    case foldr collectPostFailure [] postResult of
      xs@(_ : _) -> logWarn "Post documents failed" ["errors" .= xs]
      [] -> do
        -- Post the commit date
        res <- httpRetry "api/commit" $ commitTimestamp entity
        case res of
          Just (err :: Text) -> do
            logWarn "Commit date failed" ["err" .= err]
          Nothing -> do
            logInfo_ "Continuing on next entity"
            go

  collectPostFailure :: ProcessResult -> [Text] -> [Text]
  collectPostFailure res acc = case res of
    AddOk -> acc
    AddError err -> err : acc

  -- Adapt the document stream to intermediate representation
  getStream oldestAge entity = case documentStream of
    Changes s ->
      let project = extractEntityValue _Project
       in S.map (fmap DTChanges) (s oldestAge project)
    Projects s ->
      let organization = extractEntityValue _Organization
       in S.map (fmap DTProject) (s organization)
    TaskDatas s ->
      let td = extractEntityValue _TaskDataEntity
       in S.map (fmap DTTaskData) (s oldestAge td)
   where
    extractEntityValue prism =
      fromMaybe (error $ "Entity is not the right shape: " <> show entity) $
        preview prism entity

  -- 'mkRequest' creates the 'AddDocRequests' for a given oldest entity and a list of documenttype
  -- this is used by the processBatch function.
  mkRequest :: Entity -> [DocumentType] -> AddDocRequest
  mkRequest entity xs =
    let addDocRequestIndex = indexName
        addDocRequestCrawler = from crawlerName
        addDocRequestApikey = apiKey
        addDocRequestEntity = Just (from entity)
        -- TODO: add LentilleError here so that we don't have to throwError on Left
        -- instead we should send the problematic value to the API so that it can be reported
        -- and retried later when the code is fixed
        addDocRequestChanges = V.fromList $ mapMaybe getChanges xs
        addDocRequestEvents = V.fromList $ concat $ mapMaybe getEvents xs
        addDocRequestProjects = V.fromList $ mapMaybe getProject' xs
        addDocRequestTaskDatas = V.fromList $ mapMaybe getTD xs
        addDocRequestIssues = V.fromList $ mapMaybe getIssue xs
        addDocRequestIssueEvents = V.fromList $ concat $ mapMaybe getIssueEvent xs
     in AddDocRequest {..}
   where
    getIssue = \case
      DTIssues (issue, _) -> Just issue
      _ -> Nothing
    getIssueEvent = \case
      DTIssues (_, events) -> Just events
      _ -> Nothing
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
  (PrometheusEffect :> es, LoggerEffect :> es, Retry :> es, MonoClientEffect :> es) =>
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

-- | Remove the left part of the stream and throw an error when they occurs.
-- The error contains the first left encountered, and the rest of the stream.
eitherStreamToError ::
  Stream (Of (Either err a)) (Eff es) () ->
  Stream (Of a) (Eff (Error (err, Stream (Of (Either err a)) (Eff es) ()) : es)) ()
eitherStreamToError stream = do
  nextE <- hoist E.raise (lift (S.next stream))
  case nextE of
    -- The stream is over, stop here
    Left () -> pure ()
    Right (x, xs) -> do
      case x of
        -- TODO: should we continue after the first error?
        Left e -> lift (throwError (e, xs))
        Right v -> do
          S.yield v
          eitherStreamToError xs
