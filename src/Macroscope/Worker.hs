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
  -- test helper
  getStreamOldestEntity,
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
import Streaming.Prelude qualified as S

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
  | -- | Fetch recent changes from a user
    UserChanges (UTCTime -> Text -> LentilleStream es (Change, [ChangeEvent]))

-- | Get the entity type managed by a given stream
streamEntity :: DocumentStream es -> CrawlerPB.EntityType
streamEntity = \case
  Projects _ -> EntityTypeENTITY_TYPE_ORGANIZATION
  Changes _ -> EntityTypeENTITY_TYPE_PROJECT
  TaskDatas _ -> EntityTypeENTITY_TYPE_TASK_DATA
  UserChanges _ -> EntityTypeENTITY_TYPE_USER

-- | Get a text representation of a stream type
streamName :: DocumentStream m -> Text
streamName = \case
  Projects _ -> "Projects"
  Changes _ -> "Changes"
  TaskDatas _ -> "TaskDatas"
  UserChanges _ -> "UserChanges"

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
  | DTError CrawlerError
  deriving (Generic, ToJSON)

-- | ProcessError are produced by the processStream.
data ProcessError
  = -- | Monocle crawler commit API failed
    CommitError Text
  | -- | Monocle crawler add API failed
    AddError Text
  | -- | External API failed
    StreamError LentilleError

-- | 'processStream' read the stream of document and post to the monocle API
processStream ::
  forall es.
  -- | Funtion to log about the processing
  (Int -> Eff es ()) ->
  -- | Function to post on the Monocle API
  ([DocumentType] -> Eff es AddDocResponse) ->
  -- | The stream of documents to read
  LentilleStream es DocumentType ->
  -- | The processing results
  Eff es [Maybe ProcessError]
processStream logFunc postFunc = go (0 :: Word) [] []
 where
  go count acc results stream = do
    eDocument <- S.next stream
    case eDocument of
      Left () -> do
        -- The end of the stream
        res <- processBatch acc
        pure $ reverse (res : results)
      Right (edoc, rest) -> do
        -- We got a new document
        let doc = case edoc of
              Right x -> x
              Left err -> DTError $ toCrawlerError err
        let addStreamError :: [Maybe ProcessError] -> [Maybe ProcessError]
            addStreamError = case edoc of
              Right _ -> id
              -- This is likely an error we can't recover, so don't add stream error
              Left (LentilleError _ (PartialErrors _)) -> id
              -- Every other 'LentilleError' are fatal$
              Left err -> (Just (StreamError err) :)
        let newAcc = doc : acc
        if count == 499
          then do
            res <- processBatch newAcc
            go 0 [] (addStreamError (res : results)) rest
          else go (count + 1) newAcc (addStreamError results) rest

  toCrawlerError (LentilleError ts err) = CrawlerError {..}
   where
    crawlerErrorCreatedAt = Just $ from ts
    (crawlerErrorMessage, crawlerErrorBody) = case err of
      DecodeError xs -> ("decode", encodeJSON xs)
      RequestError e -> ("graph", encodeJSON e)
      RateLimitInfoError e -> ("rate-limit-info", encodeJSON e)
      PartialErrors es -> ("partial", encodeJSON es)

  processBatch :: [DocumentType] -> Eff es (Maybe ProcessError)
  processBatch [] = pure Nothing
  processBatch (reverse -> docs) = do
    logFunc (length docs)
    resp <- postFunc docs
    pure $ case resp of
      AddDocResponse Nothing -> Nothing
      AddDocResponse (Just err) -> Just (AddError (show err))

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
      errors <-
        runStreamError startTime apiKey indexName crawlerName documentStream offset
      forM_ errors \case
        AddError err -> logWarn "Could not add documents" ["err" .= err]
        CommitError err -> logWarn "Could not commit update date" ["err" .= err]
        StreamError err -> logWarn "Stream produced a fatal error" ["err" .= err]

-- | 'runStreamError' is the stream processor
runStreamError ::
  forall es.
  (LoggerEffect :> es, Retry :> es, PrometheusEffect :> es, MonoClientEffect :> es) =>
  UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream es ->
  Word32 ->
  Eff es [ProcessError]
runStreamError startTime apiKey indexName (CrawlerName crawlerName) documentStream offset = go
 where
  go = do
    logInfo "Looking for oldest entity" ["offset" .= offset]

    -- Query the monocle api for the oldest entity to be updated.
    oldestEntityM <- getStreamOldestEntity indexName (from crawlerName) (streamEntity documentStream) offset
    case oldestEntityM of
      Nothing -> do
        logInfo_ "Unable to find entity to update"
        pure []
      Just (oldestAge, entity)
        | -- add a 1 second delta to avoid Hysteresis
          addUTCTime 1 oldestAge >= startTime -> do
            logInfo "Crawling entities completed" ["entity" .= entity, "age" .= oldestAge]
            pure []
        | otherwise -> goStream oldestAge entity

  goStream oldestAge entity = do
    logInfo "Processing" ["entity" .= entity, "age" .= oldestAge]

    -- Run the document stream for that entity
    postResult <-
      processStream
        (\c -> logInfo "Posting documents" ["count" .= c])
        (httpRetry "api/commit/add" . mCrawlerAddDoc . mkRequest entity)
        (getStream oldestAge entity)

    case catMaybes postResult of
      [] -> do
        -- Post the commit date
        res <- httpRetry "api/commit" $ commitTimestamp entity
        case res of
          Just err -> pure [CommitError err]
          Nothing -> do
            logInfo_ "Continuing on next entity"
            go
      xs -> pure xs

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
    UserChanges s ->
      let user = extractEntityValue _User
       in S.map (fmap DTChanges) (s oldestAge user)
   where
    extractEntityValue prism =
      fromMaybe (error $ "Entity is not the right shape: " <> show entity)
        $ preview prism entity

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
        addDocRequestErrors = V.fromList $ mapMaybe getError xs
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
    getError = \case
      DTError e -> Just e
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
