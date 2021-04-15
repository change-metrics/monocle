{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- BugZilla system
module Lentille.Bugzilla (searchExpr, toTrackerData, getBZData, getBugzillaSession) where

import Data.Time (UTCTime)
import Lentille.Client (IsoTime (..), TrackerData (..))
import Lentille.Worker (LogEvent (LogGetBugs), MonadLog, MonadMask, log, retry)
import Relude
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S
import Web.Bugzilla.RedHat (BugzillaSession)
import qualified Web.Bugzilla.RedHat as BZ
import Web.Bugzilla.RedHat.Search ((.&&.), (.==.))
import qualified Web.Bugzilla.RedHat.Search as BZS

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
    changeUrl ebug = BZ.externalTypeUrl (BZ.externalType ebug) <> BZ.externalBugId ebug
    bugType =
      if "FutureFeature" `elem` BZ.bugKeywords bz
        then "RFE"
        else "BUG"
    mkTrackerData :: BZ.ExternalBug -> TrackerData
    mkTrackerData ebug =
      TrackerData
        (IsoTime . BZ.bugLastChangeTime $ bz)
        (changeUrl ebug)
        bugType
        (BZ.bugId bz)
        ("https://bugzilla.redhat.com/show_bug.cgi?id=" <> show (BZ.bugId bz))
        (BZ.bugSummary bz)
        (BZ.bugSeverity bz)
        (BZ.bugPriority bz)

getBZData :: (MonadMask m, MonadLog m, MonadIO m) => BugzillaSession -> UTCTime -> Stream (Of TrackerData) m ()
getBZData bzSession sinceTS = go 0
  where
    doGet :: MonadIO m => Int -> m [BZ.Bug]
    doGet offset =
      liftIO $ BZ.searchBugsAllWithLimit bzSession 100 offset (searchExpr sinceTS)
    go offset = do
      -- Retrieve rhbz
      bugs <- lift $ do
        log $ LogGetBugs sinceTS offset 100
        retry . doGet $ offset
      -- Create a flat stream of tracker data
      S.each (concatMap toTrackerData bugs)
      -- Keep on retrieving the rest
      unless (length bugs < 100) (go (offset + length bugs))

getBugzillaSession :: MonadIO m => Text -> m BugzillaSession
getBugzillaSession host = BZ.AnonymousSession <$> liftIO (BZ.newBugzillaContext host)
