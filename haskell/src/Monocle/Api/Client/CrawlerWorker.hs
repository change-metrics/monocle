{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker interface.
module Monocle.Api.Client.CrawlerWorker
  ( run,
    DocumentStream (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Api.Client.Api
import Monocle.Api.Client.Internal
import Monocle.Api.Client.Worker hiding (run)
import Monocle.Change (Change, ChangeEvent)
import Monocle.Crawler
import Monocle.Prelude
import Monocle.TaskData (TaskData)
import Proto3.Suite.Types (Enumerated (..))
import Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | A crawler is defined as a DocumentStream:
data DocumentStream m
  = -- | Fetch project for a organization name
    Projects (Text -> Stream (Of Text) m ())
  | -- | Fetch recent changes from a project
    Changes (UTCTime -> Text -> Stream (Of Change) m ())
  | -- | Fetch recent task data
    TaskDatas (UTCTime -> Stream (Of TaskData) m ())

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

-------------------------------------------------------------------------------
-- Worker implementation
-------------------------------------------------------------------------------

-- | The crawler stream is (locally) converted to a Stream (Of DocumentType)
-- This intermediary representation enables generic processing with 'processBatch'
data DocumentType
  = DTProject Text
  | DTChanges Change
  | DTTaskData TaskData

data ProcessResult = AddOk | AddError Text deriving stock (Show)

type OldestEntity = CommitInfoResponse_OldestEntity

-- | 'processBatch' handles the monocle api crawlerAddDoc call
processBatch :: (MonadIO m, MonadLog m) => ([DocumentType] -> m AddDocResponse) -> [DocumentType] -> m ProcessResult
processBatch postFunc docs = do
  log $ LogPostData (length docs)
  resp <- postFunc docs
  pure $ case resp of
    AddDocResponse Nothing -> AddOk
    AddDocResponse (Just err) -> AddError (show err)

-- | 'process' post to the monocle api a stream of document
process :: (MonadIO m, MonadLog m) => ([DocumentType] -> m AddDocResponse) -> Stream (Of DocumentType) m () -> m ()
process postFunc =
  S.print
    . S.mapM (processBatch postFunc)
    . S.mapped S.toList
    . S.chunksOf 500

-- | 'CompletionStatus' is the output of the 'run' function
data CompletionStatus = Completed | NotCompleted

-- | Run is the main function used by macroscope
run ::
  (MonadThrow m, MonadMask m, MonadLog m, MonadIO m) =>
  MonocleClient ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream m ->
  m CompletionStatus
run monocleClient apiKey indexName crawlerName documentStream = do
  -- It is important to get the commit date before starting the process to not miss
  -- document updated when we start
  startTime <- log' LogStarting

  -- Query the monocle api for the oldest entity to be updated.
  oldestEntity <- getOldestEntity

  -- Run the document stream for that entity
  process (crawlerAddDoc monocleClient . mkRequest oldestEntity) (getStream oldestEntity)

  -- Post the commit date
  res <- retry $ commitTimestamp oldestEntity startTime
  log (if res then LogEnded else LogFailed)

  -- TODO: return a signal when the update is incomplete, e.g. when we are ratelimited
  pure Completed
  where
    -- Adapt the document stream to intermediate representation
    getStream oldestEntity = case documentStream of
      Changes s ->
        let (untilDate, project) = getProject oldestEntity
         in S.map DTChanges (s untilDate project)
      _ -> error "Not Implemeneted"

    -- The type of the oldest entity for a given document stream
    entityType = case documentStream of
      Projects _ -> error "Not Implemeneted"
      Changes _ -> CommitInfoRequest_EntityTypeProject
      TaskDatas _ -> error "Not Implemented"

    getOldestEntity = do
      resp <-
        crawlerCommitInfo
          monocleClient
          ( CommitInfoRequest
              indexName
              crawlerName
              (Enumerated $ Right entityType)
          )
      case resp of
        CommitInfoResponse (Just (CommitInfoResponseResultEntity entity)) -> pure entity
        _ -> error $ "Could not got initial timesamp: " <> show resp

    -- 'mkRequest' creates the 'AddDocRequests' for a given oldest entity and a list of documenttype
    -- this is used by the processBatch function.
    mkRequest :: OldestEntity -> [DocumentType] -> AddDocRequest
    mkRequest oe xs =
      let addDocRequestIndex = indexName
          addDocRequestCrawler = crawlerName
          addDocRequestApikey = apiKey
          addDocRequestEntity = commitInfoResponse_OldestEntityEntity oe
          addDocRequestChanges = V.fromList $ mapMaybe getChanges xs
          addDocRequestEvents = mempty
       in AddDocRequest {..}
      where
        getChanges x = case x of
          DTChanges change -> Just change
          _ -> Nothing

    -- 'commitTimestamp' post the commit date.
    commitTimestamp oe startTime = do
      commitResp <-
        crawlerCommit
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
          putTextLn ("Commit failed: " <> show err)
          pure False
        _ -> do
          putTextLn "Empty commit response"
          pure False
