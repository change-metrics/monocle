{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle queries
-- The goal of this module is to transform 'Query' into list of items
module Monocle.Backend.Queries where

import Data.Aeson (Value (Object), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Database.Bloodhound.Raw as BHR
import Monocle.Backend.Documents (ELKChange (..))
import Monocle.Prelude
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (SortOrder (..))

-- | Helper search func that can be replaced by a scanSearch
doSearch :: (Aeson.FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m (BH.SearchResult a)
doSearch indexName search = do
  -- monocleLog . decodeUtf8 . Aeson.encode $ search
  rawResp <- BH.searchByIndex indexName search
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left _e -> handleError rawResp
    Right x -> pure x
  where
    handleError resp = do
      monocleLog (show resp)
      error "Elastic response failed"

simpleSearch :: (Aeson.FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m [BH.Hit a]
simpleSearch indexName search = BH.hits . BH.searchHits <$> doSearch indexName search

runQuery :: MonadIO m => Text -> BH.BHEnv -> Text -> Q.Query -> m [ELKChange]
runQuery docType bhEnv index queryBase =
  liftIO $
    BH.runBH bhEnv $ do
      resp <- fmap BH.hitSource <$> simpleSearch (BH.IndexName index) search
      pure $ catMaybes resp
  where
    queryBH = maybe [] ((: [])) $ Q.queryBH queryBase
    query =
      BH.QueryBoolQuery $
        BH.mkBoolQuery ([BH.TermQuery (BH.Term "type" docType) Nothing] <> queryBH) [] [] []
    search =
      (BH.mkSearch (Just query) Nothing)
        { BH.size = BH.Size (Q.queryLimit queryBase),
          BH.sortBody = toSortBody <$> Q.queryOrder queryBase
        }
    toSortBody (field', order) =
      [ BH.DefaultSortSpec
          ( BH.DefaultSort (BH.FieldName field') (sortOrder order) Nothing Nothing Nothing Nothing
          )
      ]
    sortOrder order = case order of
      Asc -> BH.Ascending
      Desc -> BH.Descending

changes :: MonadIO m => BH.BHEnv -> Text -> Q.Query -> m [ELKChange]
changes = runQuery "Change"

countEvents :: MonadIO m => BH.BHEnv -> BH.IndexName -> BH.Query -> m Word32
countEvents bhEnv index query = do
  liftIO $
    BH.runBH bhEnv $ do
      resp <- BH.countByIndex index (BH.CountQuery query)
      case resp of
        Left e -> error (show e)
        Right x -> pure (fromInteger . toInteger . BH.crCount $ x)

mkAnd :: [BH.Query] -> BH.Query
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery andQ [] [] []

-- | Add a change state filter to the query
changeState :: BH.Query -> Text -> BH.Query
changeState baseQuery state' =
  mkAnd
    [ BH.TermQuery (BH.Term "type" "Change") Nothing,
      BH.TermQuery (BH.Term "state" state') Nothing,
      baseQuery
    ]

-- | Add a document type filter to the query
documentType :: BH.Query -> Text -> BH.Query
documentType baseQuery type' =
  mkAnd
    [ BH.TermQuery (BH.Term "type" type') Nothing,
      baseQuery
    ]

-- | Handle aggregate requests
toAggRes :: BH.SearchResult Value -> BH.AggregationResults
toAggRes res = fromMaybe (error "oops") (BH.aggregations res)

aggSearch ::
  (MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Query -> BH.Aggregations -> m BH.AggregationResults
aggSearch indexName query aggs = do
  toAggRes <$> doSearch indexName (BH.mkAggregateSearch (Just query) aggs)

runAggSearch :: MonadIO m => BH.BHEnv -> BH.IndexName -> BH.Query -> BH.Aggregations -> m BH.AggregationResults
runAggSearch bhEnv index query agg = liftIO $ BH.runBH bhEnv $ aggSearch index query agg

-- | Extract a single aggregation result from the map
parseAggregationResults :: (FromJSON a) => Text -> BH.AggregationResults -> a
parseAggregationResults key res = getExn $ do
  value <- Map.lookup key res `orDie` ("No value found for: " <> toString key)
  Aeson.parseEither Aeson.parseJSON value

-- | Event counts
data EventCounts = EventCounts
  { openedCount :: Word32,
    mergedCount :: Word32,
    abandonedCount :: Word32,
    selfMergedCount :: Word32
  }
  deriving (Eq, Show)

getEventCounts :: (MonadThrow m, MonadIO m) => BH.BHEnv -> BH.IndexName -> BH.Query -> m EventCounts
getEventCounts bhEnv index query =
  EventCounts
    <$> count (queryState "OPEN")
      <*> count (queryState "MERGED")
      <*> count (queryState "ABANDONED")
      <*> count selfMergedQ
  where
    count = countEvents bhEnv index
    bhQuery = query
    queryState = changeState bhQuery
    selfMergedQ = mkAnd [query, BH.TermQuery (BH.Term "self_merged" "true") Nothing]

-- $setup
-- >>> import Data.Aeson (encode)

-- | The histo event query
-- >>> putTextLn $ decodeUtf8 $ encode histoEventAgg
-- {"date_histogram":{"interval":"day","field":"created_at"}}
data HistoEventBucket = HistoEventBucket
  { heKey :: Word64,
    heCount :: Word32
  }
  deriving (Eq, Show)

instance FromJSON HistoEventBucket where
  parseJSON (Object v) = HistoEventBucket <$> v .: "key" <*> v .: "doc_count"
  parseJSON _ = mzero

data HistoEventAgg = HistoEventAgg
  { heBuckets :: V.Vector HistoEventBucket
  }
  deriving (Eq, Show)

instance FromJSON HistoEventAgg where
  parseJSON (Object v) = HistoEventAgg <$> v .: "buckets"
  parseJSON _ = mzero

getHistoEventAgg :: (MonadThrow m, MonadIO m) => BH.BHEnv -> BH.IndexName -> BH.Query -> m HistoEventAgg
getHistoEventAgg bhEnv index query =
  parseAggregationResults "agg1"
    <$> runAggSearch bhEnv index query (Map.fromList [("agg1", histoEventAgg)])

histoEventAgg :: BH.Aggregation
histoEventAgg = BH.DateHistogramAgg dateAgg
  where
    interval = BH.Day
    dateAgg =
      BH.mkDateHistogram (BH.FieldName "created_at") interval

-- | AggValue decodes aggregations with a single value
newtype AggValue = AggValue {getValue :: Double}

instance FromJSON AggValue where
  parseJSON (Object v) = AggValue <$> v .: "value"
  parseJSON _ = mzero

getAggValue :: Text -> BH.AggregationResults -> Double
getAggValue key = getValue . parseAggregationResults key

-- | The 'changeMergedStatsDuration' result
data MergedStatsDuration = MergedStatsDuration
  { avg :: Double,
    variability :: Double
  }
  deriving (Eq, Show)

field :: Text -> Value
field name = Aeson.object ["field" .= name]

changeMergedStatsDuration ::
  (MonadThrow m, MonadIO m) => BH.BHEnv -> BH.IndexName -> BH.Query -> m MergedStatsDuration
changeMergedStatsDuration bhEnv index query = do
  res <- toAggRes <$> BHR.search bhEnv index (BHR.aggWithDocValues agg query)
  pure $ MergedStatsDuration (getAggValue "avg" res) (getAggValue "variability" res)
  where
    agg =
      [ ( "avg",
          Aeson.object
            ["avg" .= field "duration"]
        ),
        ( "variability",
          Aeson.object
            [ "median_absolute_deviation" .= field "duration"
            ]
        )
      ]

-- | The achievement query
data ProjectBucket = ProjectBucket
  { pbKey :: LText,
    pbCount :: Word32
  }
  deriving (Eq, Show)

instance FromJSON ProjectBucket where
  parseJSON (Object v) = ProjectBucket <$> v .: "key" <*> v .: "doc_count"
  parseJSON _ = mzero

newtype ProjectBuckets = ProjectBuckets {unProjectBuckets :: [ProjectBucket]} deriving (Eq, Show)

instance FromJSON ProjectBuckets where
  parseJSON (Object v) = ProjectBuckets <$> v .: "buckets"
  parseJSON _ = mzero

data EventProjectBucketAgg = EventProjectBucketAgg
  { epbType :: LText,
    epbCount :: Word32,
    epbProjects :: [ProjectBucket]
  }
  deriving (Eq, Show)

instance FromJSON EventProjectBucketAgg where
  parseJSON (Object v) =
    EventProjectBucketAgg
      <$> v .: "key"
      <*> v .: "doc_count"
      <*> (unProjectBuckets <$> v .: "project")
  parseJSON _ = mzero

newtype EventProjectBucketAggs = EventProjectBucketAggs {unEPBuckets :: [EventProjectBucketAgg]} deriving (Eq, Show)

instance FromJSON EventProjectBucketAggs where
  parseJSON (Object v) = EventProjectBucketAggs <$> v .: "buckets"
  parseJSON _ = mzero

getProjectAgg :: MonadIO m => BH.BHEnv -> BH.IndexName -> BH.Query -> m [EventProjectBucketAgg]
getProjectAgg bhEnv index query = do
  res <- toAggRes <$> BHR.search bhEnv index (BHR.aggWithDocValues agg query)
  pure $ unEPBuckets (parseAggregationResults "agg" res)
  where
    agg =
      [ ( "agg",
          Aeson.object
            [ "terms" .= field "type",
              "aggs"
                .= Aeson.object
                  [ "project"
                      .= Aeson.object
                        ["terms" .= field "repository_fullname"]
                  ]
            ]
        )
      ]
