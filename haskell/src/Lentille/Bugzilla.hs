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
    BugWithScore,
    BZ.BugzillaApikey (..),
  )
where

import Data.Aeson
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Api.Client.Worker (LogEvent (LogGetBugs), MonadLog, MonadMask, log, retry)
import Monocle.TaskData
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
bugWithScoreIncludeFieldQuery = [("include_fields", Just . toText $ intercalate "," fields)]
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

-- getBugs unwraps the 'BugsWithScore' newtype wrapper
getBugs :: (MonadIO m) => BugzillaSession -> BZ.Request -> m [BugWithScore]
getBugs bzSession request = do
  BugsWithScore bugs <- liftIO $ BZ.sendBzRequest bzSession request
  pure bugs

getBugWithScore :: (MonadIO m) => BugzillaSession -> BZ.BugId -> m BugWithScore
getBugWithScore bzSession bugId' = do
  bugs <- getBugs bzSession request
  case bugs of
    [x] -> pure x
    xs -> error $ "Got more or less than one bug " <> show xs
  where
    request = BZ.newBzRequest bzSession ["bug", show bugId'] bugWithScoreIncludeFieldQuery

getBugsWithScore ::
  (MonadIO m) =>
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
getBugsWithScore bzSession sinceTS product'' limit offset = getBugs bzSession request
  where
    request = BZ.newBzRequest bzSession ["bug"] (bugWithScoreIncludeFieldQuery <> page <> searchQuery)
    page = [("limit", Just $ show limit), ("offset", Just $ show offset), ("order", Just "changeddate")]
    searchQuery = BZS.evalSearchExpr $ (searchExpr sinceTS product'')

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
        (toLazy . changeUrl $ ebug)
        (toLazy <$> (V.fromList . bugKeywords $ bz))
        (show $ bugId bz)
        (toLazy $ "https://bugzilla.redhat.com/show_bug.cgi?id=" <> show (bugId bz))
        (toLazy $ bugSummary bz)
        (toLazy $ bugSeverity bz)
        (toLazy $ bugPriority bz)
        (fromInteger . toInteger . bugPmScore $ bz)

-- | Stream task data from a starting date by incrementing the offset until the result count is less than the limit
getBZData ::
  (MonadMask m, MonadLog m, MonadIO m) => BugzillaSession -> Text -> UTCTime -> Stream (Of TaskData) m ()
getBZData bzSession product''' sinceTS = go 0
  where
    limit = 100
    doGet :: MonadIO m => Int -> m [BugWithScore]
    doGet offset =
      liftIO $ getBugsWithScore bzSession sinceTS product''' limit offset
    go offset = do
      -- Retrieve rhbz
      bugs <- lift $ do
        log $ LogGetBugs sinceTS offset limit
        retry . doGet $ offset
      -- Create a flat stream of tracker data
      S.each (concatMap toTaskData bugs)
      -- Keep on retrieving the rest
      unless (length bugs < limit) (go (offset + length bugs))

getBugzillaSession :: MonadIO m => Text -> Maybe BZ.BugzillaApikey -> m BugzillaSession
getBugzillaSession host Nothing = BZ.AnonymousSession <$> liftIO (BZ.newBugzillaContext host)
getBugzillaSession host (Just apiKey) = flip BZ.ApikeySession apiKey <$> liftIO (BZ.newBugzillaContext host)
