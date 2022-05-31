{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- BugZilla system
module Lentille.Bugzilla
  ( searchExpr,
    toTaskData,
    getBZData,
    getBugzillaSession,
    getBugWithScore,
    getBugsWithScore,
    MonadBZ,
    BugzillaSession,
    BugWithScore,
    BZ.BugzillaApiKey (..),
    getApikey,
  )
where

import Data.Aeson
import Data.Vector qualified as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille
import Monocle.Prelude
import Monocle.Protob.Search (TaskData (..))
import Streaming.Prelude qualified as S
import Web.RedHatBugzilla (BugzillaSession)
import Web.RedHatBugzilla qualified as BZ
import Web.RedHatBugzilla.Search ((.&&.), (.==.))
import Web.RedHatBugzilla.Search qualified as BZS

class (MonadLog m, MonadRetry m, MonadMonitor m) => MonadBZ m where
  bzRequest :: FromJSON bugs => BZ.Request -> m bugs

instance MonadBZ LentilleM where
  bzRequest = liftIO . bzRequest

instance MonadBZ IO where
  bzRequest = BZ.sendBzRequest

-------------------------------------------------------------------------------
-- BugZilla system
-------------------------------------------------------------------------------
searchExpr :: UTCTime -> Text -> BZS.SearchExpression
searchExpr sinceTS product' = since .&&. linkId .&&. productField
  where
    linkId = BZS.isNotEmpty $ BZS.CustomField "ext_bz_bug_map.ext_bz_bug_id"
    productField = BZS.ProductField .==. product'
    since = BZS.changedSince sinceTS

-- | The data type containing the included fields
data BugWithScore = BugWithScore
  { bugId :: Int,
    bugLastChangeTime :: UTCTime,
    bugKeywords :: [Text],
    bugSummary :: Text,
    bugPriority :: Text,
    bugSeverity :: Text,
    bugExternalBugs :: [BZ.ExternalBug],
    bugPmScore :: Int
  }
  deriving (Show, Eq)

instance FromJSON BugWithScore where
  parseJSON (Object v) =
    do
      -- default the pm_score value to 0 when it is not set
      pmScoreM <- v .:? "cf_pm_score"
      let pmScore = case pmScoreM of
            Just str -> fromMaybe (error $ "score is not a number: " <> show str) $ readMaybe str
            Nothing -> 0
      BugWithScore
        <$> v .: "id"
        <*> v .: "last_change_time"
        <*> v .: "keywords"
        <*> v .: "summary"
        <*> v .: "priority"
        <*> v .: "severity"
        <*> v .: "external_bugs"
        <*> pure pmScore
  parseJSON _ = mzero

-- | The http query to get the BugWithScore shape
bugWithScoreIncludeFieldQuery :: [(Text, Maybe Text)]
bugWithScoreIncludeFieldQuery = [("include_fields", Just . from $ intercalate "," fields)]
  where
    fields =
      [ "id",
        "last_change_time",
        "keywords",
        "summary",
        "priority",
        "severity",
        "external_bugs",
        "cf_pm_score"
      ]

-- | A newtype wrapper to handle the {"bugs": []} layer of json the reponse
newtype BugsWithScore = BugsWithScore [BugWithScore] deriving (Eq, Show)

instance FromJSON BugsWithScore where
  parseJSON (Object v) = do
    bugs <- v .: "bugs"
    pure $ BugsWithScore bugs
  parseJSON _ = mzero

getApikey :: Text -> BZ.BugzillaApiKey
getApikey = BZ.BugzillaApiKey

-- getBugs unwraps the 'BugsWithScore' newtype wrapper
getBugs :: MonadBZ m => BZ.Request -> m [BugWithScore]
getBugs request = do
  BugsWithScore bugs <- bzRequest request
  pure bugs

getBugWithScore :: MonadBZ m => BugzillaSession -> BZ.BugId -> m BugWithScore
getBugWithScore bzSession bugId' = do
  let request = BZ.newBzRequest bzSession ["bug", show bugId'] bugWithScoreIncludeFieldQuery
  bugs <- getBugs request
  case bugs of
    [x] -> pure x
    xs -> error $ "Got more or less than one bug " <> show xs

getBugsWithScore ::
  MonadBZ m =>
  BugzillaSession ->
  -- | The last changed date
  UTCTime ->
  -- | The product name
  Text ->
  -- | The limit
  Int ->
  -- | The offset
  Int ->
  m [BugWithScore]
getBugsWithScore bzSession sinceTS product'' limit offset = do
  let request = BZ.newBzRequest bzSession ["bug"] (bugWithScoreIncludeFieldQuery <> page <> searchQuery)
      page = [("limit", Just $ show limit), ("offset", Just $ show offset), ("order", Just "changeddate")]
      searchQuery = BZS.evalSearchExpr (searchExpr sinceTS product'')
  getBugs request

-- | Convert a Bugzilla bug to TaskDatas (a bug can link many changes)
toTaskData :: BugWithScore -> [TaskData]
toTaskData bz = map mkTaskData ebugs
  where
    isOpenDev :: BZ.ExternalBug -> Bool
    isOpenDev ebug = BZ.externalBzId ebug == 85
    ebugs :: [BZ.ExternalBug]
    ebugs = filter isOpenDev (bugExternalBugs bz)
    changeUrl ebug = BZ.externalTypeUrl (BZ.externalType ebug) <> BZ.externalBugId ebug
    mkTaskData :: BZ.ExternalBug -> TaskData
    mkTaskData ebug =
      TaskData
        (Just . Timestamp.fromUTCTime . bugLastChangeTime $ bz)
        (from $ changeUrl ebug)
        (from <$> (V.fromList . bugKeywords $ bz))
        (show $ bugId bz)
        (from @Text $ "https://bugzilla.redhat.com/show_bug.cgi?id=" <> show (bugId bz))
        (from $ bugSummary bz)
        (from $ bugSeverity bz)
        (from $ bugPriority bz)
        (from $ bugPmScore bz)
        "rhbz#"

-- | Stream task data from a starting date by incrementing the offset until the result count is less than the limit
getBZData :: MonadBZ m => BugzillaSession -> UTCTime -> Text -> Stream (Of TaskData) m ()
getBZData bzSession sinceTS productName = go 0
  where
    limit = 100
    doGet :: MonadBZ m => Int -> m [BugWithScore]
    doGet = getBugsWithScore bzSession sinceTS productName limit
    go offset = do
      -- Retrieve rhbz
      bugs <- lift $ do
        mLog $ Log Unspecified (LogGetBugs sinceTS offset limit)
        httpRetry ("bz", "https://bugzilla.redhat.com/show_bug.cgi", "crawler") . doGet $ offset
      -- Create a flat stream of tracker data
      S.each (concatMap toTaskData bugs)
      -- Keep on retrieving the rest
      unless (length bugs < limit) (go (offset + length bugs))

getBugzillaSession :: Text -> Maybe BZ.BugzillaApiKey -> BugzillaSession
getBugzillaSession host Nothing = BZ.AnonymousSession host
getBugzillaSession host (Just apiKey) = BZ.ApiKeySession host apiKey
