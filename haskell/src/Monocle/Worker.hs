{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker
module Monocle.Worker
  ( run,
    TaskDataFetcher (..),
    MonadLog (..),
    LogEvent (..),

    -- * Utility function
    retry,
    MonadMask,
  )
where

import Control.Monad.Catch (Handler (Handler), MonadMask, MonadThrow)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Client
import Monocle.TaskData
import Monocle.WebApi
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as HTTP
import Proto3.Suite.Types (Enumerated (..))
import Relude
import Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------
data LogEvent
  = LogStarting
  | LogEnded
  | LogFailed
  | LogNetworkFailure Text
  | LogGetBugs UTCTime Int Int
  | LogPostData Int

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance MonadTime IO where
  getTime = liftIO getCurrentTime

logEvent :: (MonadTime m, MonadIO m) => LogEvent -> m UTCTime
logEvent ev = do
  now <- getTime
  putTextLn $ "[" <> showTime now <> "]: " <> evStr
  pure now
  where
    showTime now = toText . take 23 $ formatTime defaultTimeLocale "%F %T.%q" now
    evStr = case ev of
      LogStarting -> "Starting updates"
      LogEnded -> "Update completed"
      LogFailed -> "Commit failed"
      LogNetworkFailure msg -> "Network error: " <> msg
      LogGetBugs ts offset limit ->
        "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
      LogPostData count -> "Posting tracker data " <> show count

class Monad m => MonadLog m where
  log' :: LogEvent -> m UTCTime
  log :: LogEvent -> m ()

instance MonadLog IO where
  log' = logEvent
  log = void . log'

-- Retry 5 times network action, doubling backoff each time
retry :: (MonadMask m, MonadLog m, MonadIO m) => m a -> m a
retry action =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 5)
    [handler]
    (const action)
  where
    backoff = 500000 -- 500ms
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        log . LogNetworkFailure $ show num <> "/5 " <> loc <> " failed: " <> show ctx
        pure True
      InvalidUrlException _ _ -> pure False

-------------------------------------------------------------------------------
-- Worker implementation
-------------------------------------------------------------------------------
newtype TaskDataFetcher m = TaskDataFetcher
  { runFetcher :: UTCTime -> Stream (Of NewTaskData) m ()
  }

data ProcessResult = Amended | AmendError Text deriving stock (Show)

pattern AddSuccess :: AddResponse
pattern AddSuccess = AddResponse Nothing

pattern AddError :: Enumerated TaskDataCommitError -> AddResponse
pattern AddError err = AddResponse (Just (AddResponseResultError err))

processBatch :: (MonadIO m, MonadLog m) => ([NewTaskData] -> m AddResponse) -> [NewTaskData] -> m ProcessResult
processBatch postFunc tds = do
  log $ LogPostData (length tds)
  resp <- postFunc tds
  pure $ case resp of
    (AddSuccess) -> Amended
    (AddError err) -> AmendError (show err)
    _ -> AmendError "Unknown error"

process :: (MonadIO m, MonadLog m) => ([NewTaskData] -> m AddResponse) -> Stream (Of NewTaskData) m () -> m ()
process postFunc =
  S.print
    . S.mapM (processBatch postFunc)
    . S.mapped S.toList --   Convert to list (type is Stream (Of [NewTaskData]) m ())
    . S.chunksOf 500 --      Chop the stream (type is Stream (Stream (Of NewTaskData) m) m ())

type ApiKey = Text

type IndexName = Text

type CrawlerName = Text

pattern CommitError err = TaskDataCommitResponse (Just (TaskDataCommitResponseResultError err))

pattern CommitSuccess <- TaskDataCommitResponse (Just (TaskDataCommitResponseResultTimestamp _ts))

pattern GetLastUpdatedSuccess ts =
  TaskDataGetLastUpdatedResponse (Just (TaskDataGetLastUpdatedResponseResultTimestamp ts))

run ::
  (MonadThrow m, MonadMask m, MonadLog m, MonadIO m) =>
  MonocleClient ->
  Maybe UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  TaskDataFetcher m ->
  m ()
run monocleClient sinceM apiKey indexName crawlerName tdf = do
  startTime <- log' LogStarting
  since <- case sinceM of
    Just ts -> pure ts
    Nothing -> getTimestampFromApi
  process (retry . taskDataAdd monocleClient . mkRequest) (runFetcher tdf since)
  res <- retry $ commitTimestamp startTime
  log (if res then LogEnded else LogFailed)
  where
    commitTimestamp startTime = do
      -- setUpdatedSince monocleClient indexName crawlerName apiKey startTime
      commitResp <-
        taskDataCommit
          monocleClient
          ( TaskDataCommitRequest
              (toLazy indexName)
              (toLazy crawlerName)
              (toLazy apiKey)
              (Just $ Timestamp.fromUtcTime startTime)
          )
      case commitResp of
        CommitSuccess -> pure True
        CommitError err -> do
          putTextLn ("Commit failed: " <> show err)
          pure False
        _ -> do
          putTextLn ("Empty commit response")
          pure False
    getTimestampFromApi = do
      resp <-
        taskDataGetLastUpdated
          monocleClient
          ( TaskDataGetLastUpdatedRequest
              (toLazy indexName)
              (toLazy crawlerName)
          )
      case resp of
        GetLastUpdatedSuccess ts -> pure $ toUtcTime ts
        _ -> error $ "Could not got initial timesamp: " <> show resp
    mkRequest :: [NewTaskData] -> AddRequest
    mkRequest =
      AddRequest
        (toLazy indexName)
        (toLazy crawlerName)
        (toLazy apiKey)
        . V.fromList
