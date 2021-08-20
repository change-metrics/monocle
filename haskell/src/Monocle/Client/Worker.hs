-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker for task data. To be replaced by the new
-- general document interface, once the legacy api is rewritten.
module Monocle.Client.Worker
  ( MonadLog (..),
    MonadTime (..),
    LogEvent (..),
    logEvent,

    -- * Utility function
    getCurrentTime,
    MonadMask,
  )
where

import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Prelude

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------
data LogEvent
  = LogStarting
  | LogStartingEntity CrawlerPB.CommitInfoRequest_EntityType
  | LogEnded
  | LogFailed
  | LogNetworkFailure Text
  | LogOldestEntity CrawlerPB.CommitInfoResponse_OldestEntity
  | LogGetBugs UTCTime Int Int
  | LogPostData Int

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance MonadTime IO where
  getTime = getCurrentTime

logEvent :: (MonadTime m, MonadIO m) => LogEvent -> m UTCTime
logEvent ev = do
  now <- getTime
  putTextLn $ "[" <> showTime now <> "]: " <> evStr
  pure now
  where
    showTime now = toText . take 23 $ formatTime defaultTimeLocale "%F %T.%q" now
    evStr = case ev of
      LogStarting -> "Starting updates"
      LogStartingEntity e -> "Starting updates for " <> show e
      LogEnded -> "Update completed"
      LogFailed -> "Commit failed"
      LogNetworkFailure msg -> "Network error: " <> msg
      LogGetBugs ts offset limit ->
        "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
      LogPostData count -> "Posting tracker data " <> show count
      LogOldestEntity oe -> "Got entity " <> show oe

class Monad m => MonadLog m where
  log' :: LogEvent -> m UTCTime
  log :: LogEvent -> m ()

instance MonadLog IO where
  log' = logEvent
  log = void . log'
