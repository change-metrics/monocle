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
import Monocle.Search.Syntax (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), defaultQueryFlavor, rangeField, toBHQueryWithFlavor)
import Monocle.Servant.Env (mkFinalQuery)

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
  query <- getQueryBH
  let search =
        (BH.mkSearch query Nothing)
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

newtype Count = MkCount Word32
  deriving newtype (Show, Eq, Ord, Enum, Real, Integral)

countToWord :: Count -> Word32
countToWord (MkCount x) = x

-- | A special Num instance that prevent arithmetic underflow
instance Num Count where
  MkCount a - MkCount b
    | b > a = MkCount 0
    | otherwise = MkCount $ a - b

  MkCount a + MkCount b = MkCount $ a + b
  MkCount a * MkCount b = MkCount $ a * b
  signum (MkCount a) = MkCount $ signum a
  fromInteger x = MkCount $ fromInteger x
  abs x = x

doCountEvents :: BH.Query -> TenantM Count
doCountEvents query = do
  -- monocleLog . decodeUtf8 . Aeson.encode $ query
  index <- getIndexName
  resp <- BH.countByIndex index (BH.CountQuery query)
  case resp of
    Left e -> error (show e)
    Right x -> pure (MkCount . fromInteger . toInteger . BH.crCount $ x)

countEvents :: QueryFlavor -> [BH.Query] -> QueryM Count
countEvents qf queries = withFilter queries $ do
  bhQuery <-
    fromMaybe (error "Query shall exist because of withFilter")
      <$> getQueryBHWithFlavor qf
  liftTenantM $ doCountEvents bhQuery

countEvents' :: [BH.Query] -> QueryM Count
countEvents' = countEvents defaultQueryFlavor

-- | The change created / review ratio
changeReviewRatio :: QueryM Float
changeReviewRatio = do
  commitCount <- countEvents qf [documentType $ "ChangeCreatedEvent" :| []]
  reviewCount <-
    countEvents
      qf
      [ documentType $ fromList ["ChangeReviewedEvent", "ChangeCommentedEvent"]
      ]
  let total, commitCountF, reviewCountF :: Float
      total = reviewCountF + commitCountF
      reviewCountF = fromIntegral reviewCount
      commitCountF = fromIntegral commitCount
  pure (if total > 0 then reviewCountF * 100 / total else -1)
  where
    -- Author makes query author match the change event author, not the receiver of the event.
    -- CreatedAt is necessary for change event.
    qf = QueryFlavor Author CreatedAt

mkAnd :: [BH.Query] -> BH.Query
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery andQ [] [] []

mkOr :: [BH.Query] -> BH.Query
mkOr orQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [] orQ

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term name value) Nothing

-- | Add a change state filter to the query
changeState :: Text -> [BH.Query]
changeState state' =
  [ BH.TermQuery (BH.Term "type" "Change") Nothing,
    BH.TermQuery (BH.Term "state" state') Nothing
  ]

-- | Add a document type filter to the query
documentType :: NonEmpty Text -> BH.Query
documentType = BH.TermsQuery "type"

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
  { openedCount :: Count,
    mergedCount :: Count,
    abandonedCount :: Count,
    selfMergedCount :: Count
  }
  deriving (Eq, Show)

getEventCounts :: QueryM EventCounts
getEventCounts =
  -- TODO: ensure the right flavor is used
  EventCounts
    <$> countEvents' (changeState "OPEN")
      <*> countEvents' (changeState "MERGED")
      <*> countEvents' (changeState "ABANDONED")
      <*> countEvents' selfMergedQ
  where
    selfMergedQ = [BH.TermQuery (BH.Term "self_merged" "true") Nothing]

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

changeMergedStatsDuration :: [BH.Query] -> TenantM MergedStatsDuration
changeMergedStatsDuration query = do
  index <- getIndexName
  let finalQuery = mkFinalQuery query
  res <- toAggRes <$> BHR.search index (BHR.aggWithDocValues agg finalQuery)
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
  res <- toAggRes <$> BHR.search index (BHR.aggWithDocValues agg (Just query))
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

getQueryFromSL :: Text -> [BH.Query]
getQueryFromSL query =
  flip Q.queryBH defaultQueryFlavor $ Q.load Nothing mempty Nothing query

-- | TopTerm agg query utils
getSimpleTR :: BH.TermsResult -> TermResult
getSimpleTR tr = TermResult (getTermKey tr) (BH.termsDocCount tr)

data TermResult = TermResult {trTerm :: Text, trCount :: Int} deriving (Show, Eq)

getTermKey :: BH.TermsResult -> Text
getTermKey (BH.TermsResult (BH.TextValue tv) _ _) = tv
getTermKey BH.TermsResult {} = error "Unexpected match"

getTermsAgg :: BH.Query -> Text -> Maybe Int -> TenantM [BH.TermsResult]
getTermsAgg query onTerm maxBuckets = do
  search <- aggSearch query aggs
  pure $ filter isNotEmptyTerm $ unfilteredR search
  where
    aggs =
      BH.mkAggregations "singleTermAgg" $
        BH.TermsAgg $
          (BH.mkTermsAggregation onTerm)
            { BH.termSize = maxBuckets
            }
    unfilteredR search' = maybe [] BH.buckets (BH.toTerms "singleTermAgg" search')
    -- Terms agg returns empty terms in a buckets
    isNotEmptyTerm :: BH.TermsResult -> Bool
    isNotEmptyTerm tr = getTermKey tr /= ""

getDocTypeTopCountByField :: NonEmpty Text -> Text -> Maybe Int -> QueryFlavor -> QueryM [TermResult]
getDocTypeTopCountByField doctype attr size qflavor = do
  -- Prepare the query
  basequery <- toBHQueryWithFlavor qflavor <$> getQuery
  let query = mkAnd $ basequery <> [documentType doctype]

  -- Run the aggregation
  results <- liftTenantM (runTermAgg query size)

  -- Return the result
  pure $ getSimpleTR <$> results
  where
    runTermAgg query = getTermsAgg query attr

-- | The repos_summary query
getRepos :: QueryM [TermResult]
getRepos =
  getDocTypeTopCountByField
    ("Change" :| [])
    "repository_fullname"
    (Just 5000)
    (QueryFlavor Author CreatedAt)

data RepoSummary = RepoSummary
  { fullname :: Text,
    totalChanges :: Count,
    abandonedChanges :: Count,
    mergedChanges :: Count,
    openChanges :: Count
  }
  deriving (Show, Eq)

getReposSummary :: QueryM [RepoSummary]
getReposSummary = do
  names <- fmap trTerm <$> getRepos
  traverse getRepoSummary names
  where
    getRepoSummary fn = withFilter (getQueryFromSL ("repo: " <> fn)) $ do
      -- Prepare the queries
      let eventQF = QueryFlavor OnAuthor CreatedAt
          changeQF = QueryFlavor Author UpdatedAt

      -- Count the events
      totalChanges' <- countEvents eventQF [documentType $ "ChangeCreatedEvent" :| []]
      openChanges' <- countEvents changeQF $ changeState "OPEN"
      mergedChanges' <- countEvents eventQF [documentType $ "ChangeMergedEvent" :| []]

      -- Return summary
      let abandonedChanges' = totalChanges' - (openChanges' + mergedChanges')
      pure $ RepoSummary fn totalChanges' abandonedChanges' mergedChanges' openChanges'

-- | get authors tops
getMostActiveAuthorByChangeCreated :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeCreated limit =
  getDocTypeTopCountByField
    ("ChangeCreatedEvent" :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostActiveAuthorByChangeMerged :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeMerged limit =
  getDocTypeTopCountByField
    ("ChangeMergedEvent" :| [])
    "on_author.muid"
    (Just limit)
    (QueryFlavor OnAuthor CreatedAt)

getMostActiveAuthorByChangeReviewed :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeReviewed limit =
  getDocTypeTopCountByField
    ("ChangeReviewedEvent" :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostActiveAuthorByChangeCommented :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeCommented limit =
  getDocTypeTopCountByField
    ("ChangeCommentedEvent" :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostReviewedAuthor :: Int -> QueryM [TermResult]
getMostReviewedAuthor limit =
  getDocTypeTopCountByField
    ("ChangeReviewedEvent" :| [])
    "on_author.muid"
    (Just limit)
    (QueryFlavor OnAuthor CreatedAt)

getMostCommentedAuthor :: Int -> QueryM [TermResult]
getMostCommentedAuthor limit =
  getDocTypeTopCountByField
    ("ChangeCommentedEvent" :| [])
    "on_author.muid"
    (Just limit)
    (QueryFlavor OnAuthor CreatedAt)

-- | peer strength authors
data PeerStrengthResult = PeerStrengthResult
  { psrAuthor :: Text,
    psrPeer :: Text,
    psrStrength :: Word32
  }
  deriving (Show, Eq)

instance Ord PeerStrengthResult where
  (PeerStrengthResult _ _ x) `compare` (PeerStrengthResult _ _ y) =
    x `compare` y

getAuthorsPeersStrength :: Word32 -> QueryM [PeerStrengthResult]
getAuthorsPeersStrength limit = do
  peers <-
    getDocTypeTopCountByField
      eventTypes
      "author.muid"
      (Just 5000)
      qf
  authors_peers <- traverse (getAuthorPeers . trTerm) peers
  pure $
    take (fromInteger $ toInteger limit) $
      reverse $
        sort $
          filter (\psr -> psrAuthor psr /= psrPeer psr) $
            concatMap transform authors_peers
  where
    eventTypes :: NonEmpty Text
    eventTypes = fromList ["ChangeReviewedEvent", "ChangeCommentedEvent"]
    qf = QueryFlavor Author CreatedAt
    getAuthorPeers :: Text -> QueryM (Text, [TermResult])
    getAuthorPeers peer = withFilter (getQueryFromSL $ "author: " <> peer) $ do
      change_authors <-
        getDocTypeTopCountByField
          eventTypes
          "on_author.muid"
          (Just 5000)
          qf
      pure (peer, change_authors)
    transform :: (Text, [TermResult]) -> [PeerStrengthResult]
    transform (peer, change_authors) = toPSR <$> change_authors
      where
        toPSR tr =
          PeerStrengthResult
            (trTerm tr)
            peer
            (fromInteger $ toInteger (trCount tr))

-- | getReviewHisto
getHisto :: QueryFlavor -> QueryM (V.Vector HistoEventBucket)
getHisto qf = do
  query <- getQuery
  queryBH <- getQueryBHWithFlavor qf

  let (minBound', maxBound') = Q.queryBounds query
      bound = Aeson.object ["min" .= minBound', "max" .= maxBound']
      date_histo =
        Aeson.object
          [ "field" .= (rangeField $ qfRange qf),
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
            "query" .= fromMaybe (error "need query") queryBH
          ]

  liftTenantM $ do
    index <- getIndexName
    res <- toAggRes <$> BHR.search index search
    pure $ heBuckets $ parseAggregationResults "agg1" res
