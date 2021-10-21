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
import Monocle.Client (MonocleClient)
import Monocle.Crawler
import Monocle.Prelude
import Monocle.Project (Project)
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
    TaskDatas (UTCTime -> LentilleStream m TaskData)

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

-- | Run is the main function used by macroscope
runStream ::
  (MonadError LentilleError m, MonadLog m, MonadRetry m, MonadCrawler m) =>
  MonocleClient ->
  MonocleTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream m ->
  m ()
runStream monocleClient startDate apiKey indexName crawlerName documentStream = drainEntities (0 :: Word32)
  where
    lc = LogCrawlerContext (toText indexName) (toText crawlerName)
    wLog event = mLog $ Log Macroscope event
    drainEntities offset =
      safeDrainEntities offset `catchError` handleStreamError offset
    safeDrainEntities offset = do
      -- It is important to get the commit date before starting the process to not miss
      -- document updated when we start
      startTime <- mGetCurrentTime
      wLog $ LogMacroRequestOldestEntity lc (streamType documentStream)

      -- Query the monocle api for the oldest entity to be updated.
      entityM <- retry $ getOldestEntity offset
      -- In case that no entity found we throw an error
      entity <- maybe (throwError NoEntity) pure entityM

      let (eType, eName) = oldestEntityEntityToText entity
          processLogFunc c = Log Macroscope $ LogMacroPostData lc eName c
      wLog $ LogMacroGotOldestEntity lc (eType, eName) (oldestEntityDate entity)

      if toMonocleTime (oldestEntityDate entity) >= startDate
        then wLog $ LogMacroEnded lc
        else do
          -- Run the document stream for that entity
          postResult <-
            process
              processLogFunc
              (retry . mCrawlerAddDoc monocleClient . mkRequest entity)
              (getStream entity)
          case foldr collectPostFailure [] postResult of
            [] -> do
              -- Post the commit date
              res <- retry $ commitTimestamp entity startTime
              if not res
                then wLog $ LogMacroCommitFailed lc
                else do
                  wLog $ LogMacroContinue lc
                  drainEntities offset
            xs -> wLog $ LogMacroPostDataFailed lc xs

    handleStreamError offset err = do
      -- TODO: report decoding error
      case err of
        DecodeError decodingErrors -> wLog $ LogMacroStreamError lc decodingErrors
        NoEntity -> wLog $ LogMacroNoOldestEnity lc
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
        let untilDate = getDate oldestEntity
         in S.map DTTaskData (s untilDate)

    -- Get a stream representation of a stream type
    streamType = \case
      Projects _ -> "Projects"
      Changes _ -> "Changes"
      TaskDatas _ -> "TaskDatas"

    getOldestEntity :: MonadCrawler m => Word32 -> m (Maybe CommitInfoResponse_OldestEntity)
    getOldestEntity offset = do
      resp <-
        mCrawlerCommitInfo
          monocleClient
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
    commitTimestamp oe startTime = do
      commitResp <-
        mCrawlerCommit
          monocleClient
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
