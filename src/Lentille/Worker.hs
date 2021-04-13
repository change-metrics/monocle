{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker
module Lentille.Worker
  ( run,
    getBugzillaSession,
    searchExpr,
    toTrackerData,
    getBZData,
    TrackerDataFetcher (..),
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (decodeFileStrict)
import Data.Text as T (null)
import Data.Time (UTCTime, getCurrentTime)
import Lentille.Client
import Relude
import Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.Directory (doesFileExist)
import Web.Bugzilla.RedHat (BugzillaSession)
import qualified Web.Bugzilla.RedHat as BZ
import Web.Bugzilla.RedHat.Search ((.&&.), (.==.))
import qualified Web.Bugzilla.RedHat.Search as BZS

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------
data LogEvent
  = LogStarting
  | LogEnded
  | LogGetBugs UTCTime Int Int

logEvent :: MonadIO m => LogEvent -> m ()
logEvent ev = do
  now <- liftIO getCurrentTime
  putTextLn $ "[" <> show now <> "]: " <> evStr
  where
    evStr = case ev of
      LogStarting -> "Starting updates"
      LogEnded -> "Update completed"
      LogGetBugs ts offset limit ->
        "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit

class Monad m => MonadLog m where
  log :: LogEvent -> m ()

instance MonadLog IO where
  log = logEvent

-------------------------------------------------------------------------------
-- BugZilla system
-------------------------------------------------------------------------------
searchExpr :: UTCTime -> BZS.SearchExpression
searchExpr sinceTS = since .&&. linkId .&&. productField
  where
    linkId = BZS.isNotEmpty $ BZS.CustomField "ext_bz_bug_map.ext_bz_bug_id"
    productField = BZS.ProductField .==. "Red Hat OpenStack"
    since = BZS.changedSince sinceTS

toTrackerData :: BZ.Bug -> [TrackerData]
toTrackerData bz = map mkTrackerData ebugs
  where
    isOpenDev :: BZ.ExternalBug -> Bool
    isOpenDev ebug = BZ.externalBzId ebug == 85
    ebugs :: [BZ.ExternalBug]
    ebugs = case BZ.bugExternalBugs bz of
      Just xs -> filter isOpenDev xs
      Nothing -> []
    mkTrackerData :: BZ.ExternalBug -> TrackerData
    mkTrackerData ebug =
      TrackerData
        (BZ.bugLastChangeTime bz)
        ( BZ.externalTypeUrl (BZ.externalType ebug)
            <> show (BZ.externalId ebug)
        )
        ("https://bugzilla.redhat.com/show_bug.cgi?id=" <> show (BZ.bugId bz))
        (BZ.bugSummary bz)
        (BZ.bugId bz)

getBZData :: MonadIO m => BugzillaSession -> UTCTime -> Stream (Of TrackerData) m ()
getBZData bzSession sinceTS = do
  liftIO $ log $ LogGetBugs sinceTS 0 5
  cacheExist <- liftIO $ doesFileExist ".cache"
  bugs <-
    liftIO
      ( if cacheExist
          then fromMaybe [] <$> decodeFileStrict ".cache"
          else BZ.searchBugsAllWithLimit bzSession 5 0 (searchExpr sinceTS)
      )
  S.each (concatMap toTrackerData bugs)

getBugzillaSession :: MonadIO m => m BugzillaSession
getBugzillaSession = BZ.AnonymousSession <$> liftIO (BZ.newBugzillaContext "bugzilla.redhat.com")

newtype TrackerDataFetcher m = TrackerDataFetcher
  { runFetcher :: UTCTime -> Stream (Of TrackerData) m ()
  }

-------------------------------------------------------------------------------
-- Worker implementation
-------------------------------------------------------------------------------
data ProcessResult = Amended | AmenError Text deriving stock (Show)

processBatch :: MonadIO m => ([TrackerData] -> m ()) -> [TrackerData] -> m ProcessResult
processBatch postFunc tds = do
  putTextLn $ "Processing: " <> show (length tds)
  postFunc tds
  pure Amended

process :: (MonadIO m) => ([TrackerData] -> m ()) -> Stream (Of TrackerData) m () -> m ()
process postFunc =
  S.print
    . S.mapM (processBatch postFunc)
    . S.mapped S.toList --   Convert to list (type is Stream (Of [TrackerData]) m ())
    . S.chunksOf 10 --       Chop the stream (type is Stream (Stream (Of TrackerData) m) m ())

run ::
  (MonadThrow m, MonadLog m, MonadIO m) =>
  MonocleClient ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  TrackerDataFetcher m ->
  m ()
run monocleClient apiKey indexName crawlerName tdf = do
  log LogStarting
  since <- getUpdatedSince monocleClient indexName crawlerName
  process (postTrackerData monocleClient indexName crawlerName apiKey) (runFetcher tdf since)
  log LogEnded
