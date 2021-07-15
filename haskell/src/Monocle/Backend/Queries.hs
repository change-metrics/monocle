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
import qualified Monocle.Search as SearchPB
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), toBHQueryWithFlavor)

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

changes :: Maybe SearchPB.Order -> Word32 -> QueryM [ELKChange]
changes orderM limit = withFilter [BH.TermQuery (BH.Term "type" "Change") Nothing] $ do
  query <- getQuery
  let search =
        (BH.mkSearch (Q.queryBH query) Nothing)
          { BH.size = BH.Size (if limit > 0 then limitInt else 50),
            BH.sortBody = toSortBody <$> orderM
          }
  liftTenantM $ do
    index <- getIndexName
    resp <- fmap BH.hitSource <$> simpleSearch index search
    pure $ catMaybes resp
  where
    limitInt = fromInteger . toInteger $ limit
    toSortBody SearchPB.Order {..} =
      let field' = BH.FieldName $ toStrict orderField
          order = sortOrder orderDirection
       in [ BH.DefaultSortSpec
              ( BH.DefaultSort field' order Nothing Nothing Nothing Nothing
              )
          ]
    sortOrder order = case fromPBEnum order of
      SearchPB.Order_DirectionASC -> BH.Ascending
      SearchPB.Order_DirectionDESC -> BH.Descending

countEvents :: BH.Query -> TenantM Word32
countEvents query = do
  -- monocleLog . decodeUtf8 . Aeson.encode $ query
  index <- getIndexName
  resp <- BH.countByIndex index (BH.CountQuery query)
  case resp of
    Left e -> error (show e)
    Right x -> pure (fromInteger . toInteger . BH.crCount $ x)

-- | The change created / review ratio
changeReviewRatio :: QueryM Float
changeReviewRatio = do
  query <- getQueryBH'
  liftTenantM $ do
    commitCount <- countEvents (documentType query "ChangeCreatedEvent")
    reviewCount <- countEvents (documentType query "ChangeReviewedEvent")
    commentCount <- countEvents (documentType query "ChangeCommentedEvent")
    let total, commitCountF, reviewCountF :: Float
        total = reviewCountF + commitCountF
        reviewCountF = fromIntegral $ reviewCount + commentCount
        commitCountF = fromIntegral $ commitCount
    pure (reviewCountF * 100 / total)

mkAnd :: [BH.Query] -> BH.Query
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery andQ [] [] []

mkOr :: [BH.Query] -> BH.Query
mkOr orQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [] orQ

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term name value) Nothing

-- | Add a change state filter to the query
changeState :: BH.Query -> Text -> BH.Query
changeState baseQuery state' =
  mkAnd
    [ BH.TermQuery (BH.Term "type" "Change") Nothing,
      BH.TermQuery (BH.Term "state" state') Nothing,
      baseQuery
    ]

-- | Add a document type filter to the query
documentType :: [BH.Query] -> Text -> BH.Query
documentType baseQuery type' =
  mkAnd
    ([BH.TermQuery (BH.Term "type" type') Nothing] <> baseQuery)

-- | User query
toUserTerm :: Text -> BH.Query
toUserTerm user = BH.TermQuery (BH.Term "author.muid" user) Nothing

-- | Handle aggregate requests
toAggRes :: BH.SearchResult Value -> BH.AggregationResults
toAggRes res = fromMaybe (error "oops") (BH.aggregations res)

aggSearch :: BH.Query -> BH.Aggregations -> TenantM BH.AggregationResults
aggSearch query aggs = do
  indexName <- getIndexName
  toAggRes <$> doSearch indexName (BH.mkAggregateSearch (Just query) aggs)

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

getEventCounts :: BH.Query -> TenantM EventCounts
getEventCounts query =
  EventCounts
    <$> countEvents (queryState "OPEN")
      <*> countEvents (queryState "MERGED")
      <*> countEvents (queryState "ABANDONED")
      <*> countEvents selfMergedQ
  where
    bhQuery = query
    queryState = changeState bhQuery
    selfMergedQ = mkAnd [query, BH.TermQuery (BH.Term "self_merged" "true") Nothing]

-- $setup
-- >>> import Data.Aeson.Encode.Pretty (encodePretty', Config (..), defConfig, compare)

-- | The histo event query
-- >>> :{
--  let
--    conf = defConfig {confCompare = compare}
--  in putTextLn $ decodeUtf8 $ encodePretty' conf histoEventAgg
-- :}
-- {
--     "date_histogram": {
--         "field": "created_at",
--         "interval": "day"
--     }
-- }
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

getHistoEventAgg :: BH.Query -> TenantM HistoEventAgg
getHistoEventAgg query =
  parseAggregationResults "agg1"
    <$> aggSearch query (Map.fromList [("agg1", histoEventAgg)])

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

changeMergedStatsDuration :: BH.Query -> TenantM MergedStatsDuration
changeMergedStatsDuration query = do
  index <- getIndexName
  res <- toAggRes <$> BHR.search index (BHR.aggWithDocValues agg query)
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

getProjectAgg :: BH.Query -> TenantM [EventProjectBucketAgg]
getProjectAgg query = do
  index <- getIndexName
  res <- toAggRes <$> BHR.search index (BHR.aggWithDocValues agg query)
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

-- | The repos_summary query
getQueryFromSL :: Text -> [BH.Query]
getQueryFromSL query = case Q.queryBH $ Q.load Nothing mempty Nothing query of
  Just q -> [q]
  Nothing -> []

getTermKey :: BH.TermsResult -> Text
getTermKey (BH.TermsResult (BH.TextValue tv) _ _) = tv
getTermKey BH.TermsResult {} = error "Unexpected match"

getTermsAgg :: BH.Query -> Text -> TenantM [BH.TermsResult]
getTermsAgg query onTerm = do
  search <- aggSearch query aggs
  pure $ filter isNotEmptyTerm $ unfilteredR search
  where
    aggs = BH.mkAggregations "singleTermAgg" $ BH.TermsAgg $ BH.mkTermsAggregation onTerm
    unfilteredR search' = maybe [] BH.buckets (BH.toTerms "singleTermAgg" search')
    -- Terms agg returns empty terms in a buckets
    isNotEmptyTerm :: BH.TermsResult -> Bool
    isNotEmptyTerm tr = getTermKey tr /= ""

data TermResult = TermResult {term :: Text, count :: Int} deriving (Show, Eq)

getRepos :: QueryM [TermResult]
getRepos = do
  -- Prepare the query
  basequery <- toBHQueryWithFlavor (QueryFlavor Author CreatedAt) <$> getQuery
  let query = mkAnd $ basequery <> getQueryE

  -- Run the aggregation
  results <- liftTenantM (runTermAgg query)

  -- Return the result
  pure $ getSimpleTR <$> results
  where
    getQueryE = getQueryFromSL "repo_regex: .*"
    runTermAgg query = getTermsAgg query "repository_fullname"
    getSimpleTR tr = TermResult (getTermKey tr) (BH.termsDocCount tr)

data RepoSummary = RepoSummary
  { fullname :: Text,
    totalChanges :: Word32,
    abandonedChanges :: Word32,
    mergedChanges :: Word32,
    openChanges :: Word32
  }
  deriving (Show, Eq)

getReposSummary :: QueryM [RepoSummary]
getReposSummary = do
  names <- fmap term <$> getRepos
  traverse getRepoSummary names
  where
    getRepoSummary fn = do
      -- Prepare the queries
      basequery <- toBHQueryWithFlavor (QueryFlavor OnAuthor UpdatedAt) <$> getQuery
      let query = mkAnd $ basequery <> getQueryFromSL ("repo: " <> fn)
          countEvent docType = liftTenantM $ countEvents (documentType [query] docType)

      -- Count the events
      totalChanges' <- countEvent "ChangeCreatedEvent"
      abandonedChanges' <- countEvent "ChangeClosedEvent"
      mergedChanges' <- countEvent "ChangeMergedEvent"

      -- Return summary
      let openChanges' = totalChanges' - (abandonedChanges' + mergedChanges')
      pure $ RepoSummary fn totalChanges' abandonedChanges' mergedChanges' openChanges'

-- | getReviewHisto
getReviewHisto :: QueryM (V.Vector HistoEventBucket)
getReviewHisto = do
  query <- getQuery

  let (minBound', maxBound') = Q.queryBounds query
      bound = Aeson.object ["min" .= minBound', "max" .= maxBound']
      date_histo =
        Aeson.object
          [ "field" .= ("created_at" :: Text),
            "calendar_interval" .= ("day" :: Text),
            "min_doc_count" .= (0 :: Word),
            "extended_bounds" .= bound
          ]
      agg =
        Aeson.object
          [ "agg1" .= Aeson.object ["date_histogram" .= date_histo]
          ]
      search =
        Aeson.object
          [ "aggregations" .= agg,
            "size" .= (0 :: Word),
            "query" .= fromMaybe (error "need query") (Q.queryBH query)
          ]

  liftTenantM $ do
    index <- getIndexName
    res <- toAggRes <$> BHR.search index search
    pure $ heBuckets $ parseAggregationResults "agg1" res
