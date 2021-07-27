-- | Monocle queries
-- The goal of this module is to transform 'Query' into list of items
module Monocle.Backend.Queries where

import Data.Aeson (Value (Object), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Database.Bloodhound.Raw as BHR
import Monocle.Backend.Documents (ELKChange (..), ELKChangeEvent (..), ELKChangeState (..), ELKDocType (..), authorMuid, changeStateToText, docTypeToText)
import Monocle.Env
import Monocle.Prelude
import qualified Monocle.Search as SearchPB
import Monocle.Search.Query (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), defaultQueryFlavor, rangeField, toBHQueryWithFlavor)
import qualified Monocle.Search.Query as Q

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

doCount :: BH.Query -> TenantM Count
doCount query = do
  -- monocleLog . decodeUtf8 . Aeson.encode $ query
  index <- getIndexName
  resp <- BH.countByIndex index (BH.CountQuery query)
  case resp of
    Left e -> error $ show e
    Right x -> pure $ naturalToCount (BH.crCount x)

countDocs :: QueryFlavor -> QueryM Count
countDocs qf = do
  bhQuery <-
    fromMaybe (error "Need a query to count") <$> getQueryBHWithFlavor qf
  liftTenantM $ doCount bhQuery

countDocs' :: QueryM Count
countDocs' = countDocs defaultQueryFlavor

-- | The change created / review ratio
changeReviewRatio :: QueryM Float
changeReviewRatio = do
  commitCount <- withFilter [documentType ElkChangeCreatedEvent] $ countDocs qf
  reviewCount <-
    withFilter [documentTypes $ fromList [ElkChangeReviewedEvent, ElkChangeCommentedEvent]] $
      countDocs qf
  let total, commitCountF, reviewCountF :: Float
      total = reviewCountF + commitCountF
      reviewCountF = fromIntegral reviewCount
      commitCountF = fromIntegral commitCount
  pure (if total > 0 then reviewCountF * 100 / total else -1)
  where
    -- Author makes query author match the change event author, not the receiver of the event.
    -- CreatedAt is necessary for change event.
    qf = QueryFlavor Author CreatedAt

-- | Add a change state filter to the query
changeState :: ELKChangeState -> [BH.Query]
changeState state' =
  [ BH.TermQuery (BH.Term "type" "Change") Nothing,
    BH.TermQuery (BH.Term "state" $ changeStateToText state') Nothing
  ]

selfMerged :: [BH.Query]
selfMerged =
  [ BH.TermQuery (BH.Term "type" "Change") Nothing,
    BH.TermQuery (BH.Term "self_merged" "true") Nothing
  ]

testIncluded :: BH.Query
testIncluded =
  BH.QueryRegexpQuery $
    BH.RegexpQuery (BH.FieldName "changed_files.path") (BH.Regexp ".*[Tt]est.*") BH.AllRegexpFlags Nothing

-- | Add a document type filter to the query
documentTypes :: NonEmpty ELKDocType -> BH.Query
documentTypes doc = BH.TermsQuery "type" $ toText . docTypeToText <$> doc

documentType :: ELKDocType -> BH.Query
documentType x = documentTypes (x :| [])

-- | User query
toUserTerm :: Text -> BH.Query
toUserTerm user = BH.TermQuery (BH.Term "author.muid" user) Nothing

-- | Handle aggregate requests
toAggRes :: BH.SearchResult Value -> BH.AggregationResults
toAggRes res = fromMaybe (error "oops") (BH.aggregations res)

aggSearch :: Maybe BH.Query -> BH.Aggregations -> TenantM BH.AggregationResults
aggSearch query aggs = do
  indexName <- getIndexName
  toAggRes <$> doSearch indexName (BH.mkAggregateSearch query aggs)

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
    <$> withFilter (changeState ElkChangeOpen) countDocs'
      <*> withFilter (changeState ElkChangeMerged) countDocs'
      <*> withFilter (changeState ElkChangeClosed) countDocs'
      <*> withFilter selfMergedQ countDocs'
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
    heDate :: LText,
    heCount :: Word32
  }
  deriving (Eq, Show)

instance FromJSON HistoEventBucket where
  parseJSON (Object v) =
    HistoEventBucket
      <$> v .: "key" <*> v .: "key_as_string" <*> v .: "doc_count"
  parseJSON _ = mzero

newtype HistoEventAgg = HistoEventAgg
  { heBuckets :: V.Vector HistoEventBucket
  }
  deriving (Eq, Show)

instance FromJSON HistoEventAgg where
  parseJSON (Object v) = HistoEventAgg <$> v .: "buckets"
  parseJSON _ = mzero

getHistoEventAgg :: BH.Query -> TenantM HistoEventAgg
getHistoEventAgg query =
  parseAggregationResults "agg1"
    <$> aggSearch (Just query) (Map.fromList [("agg1", histoEventAgg)])

histoEventAgg :: BH.Aggregation
histoEventAgg = BH.DateHistogramAgg dateAgg
  where
    interval = BH.Day
    dateAgg =
      BH.mkDateHistogram (BH.FieldName "created_at") interval

-- | Author histo
data HistoAuthorBucket = HistoAuthorBucket
  { habKey :: LText,
    habCount :: Word32
  }
  deriving (Eq, Show)

instance FromJSON HistoAuthorBucket where
  parseJSON (Object v) =
    HistoAuthorBucket <$> v .: "key" <*> v .: "doc_count"
  parseJSON _ = mzero

data HistoAuthors = HistoAuthors
  { haBuckets :: V.Vector HistoAuthorBucket
  }
  deriving (Eq, Show)

instance FromJSON HistoAuthors where
  parseJSON (Object v) = HistoAuthors <$> v .: "buckets"
  parseJSON _ = mzero

instance BucketName HistoAuthors where
  bucketName _ = "authors"

data HistoBucket a = HistoBucket
  { hbKey :: Word64,
    hbDate :: LText,
    hbCount :: Word32,
    hbSubBuckets :: a
  }
  deriving (Eq, Show)

class BucketName a where
  bucketName :: Proxy a -> Text

newtype NoSubBucket = NoSubBucket (Maybe Void)

instance FromJSON NoSubBucket where
  parseJSON _ = mzero

type HistoSimple = HistoBucket NoSubBucket

instance BucketName NoSubBucket where
  bucketName _ = "unused"

instance (FromJSON a, BucketName a) => FromJSON (HistoBucket a) where
  parseJSON (Object v) =
    HistoBucket
      <$> v .: "key" <*> v .: "key_as_string" <*> v .: "doc_count" <*> v .: (bucketName (Proxy @a))
  parseJSON _ = mzero

newtype HistoAgg a = HistoAgg
  { hBuckets :: V.Vector (HistoBucket a)
  }
  deriving (Eq, Show)

instance (FromJSON a, BucketName a) => FromJSON (HistoAgg a) where
  parseJSON (Object v) = HistoAgg <$> v .: "buckets"
  parseJSON _ = mzero

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

data FirstEvent = FirstEvent
  { feChangeCreatedAt :: UTCTime,
    feCreatedAt :: UTCTime,
    feAuthor :: LText
  }
  deriving (Show)

firstEventDuration :: FirstEvent -> Pico
firstEventDuration FirstEvent {..} = elapsedSeconds feChangeCreatedAt feCreatedAt

firstEventAverageDuration :: [FirstEvent] -> Word32
firstEventAverageDuration = truncate . fromMaybe 0 . average . map firstEventDuration

firstEventOnChanges :: QueryM [FirstEvent]
firstEventOnChanges = do
  query <- getQueryBHWithFlavor (QueryFlavor Author CreatedAt)
  (minDate, _) <- Q.queryBounds <$> getQuery
  let search = (BH.mkSearch query Nothing) {BH.size = BH.Size 10000}

  -- Collect all the events
  result <- liftTenantM $ do
    index <- getIndexName
    resp <- fmap BH.hitSource <$> BH.scanSearch index search
    pure $ catMaybes resp

  -- Group by change_id
  let changeMap :: [NonEmpty ELKChangeEvent]
      changeMap = HM.elems $ groupBy elkchangeeventChangeId result

  -- Remove old change where we may not have the first event
  let keepRecent :: NonEmpty ELKChangeEvent -> Bool
      keepRecent (ELKChangeEvent {..} :| _)
        | elkchangeeventOnCreatedAt > minDate = True
        | otherwise = False

  now <- liftIO getCurrentTime

  -- For each change, get the detail of the first event
  pure $ foldr toFirstEvent (initEvent now) <$> filter keepRecent changeMap
  where
    initEvent :: UTCTime -> FirstEvent
    initEvent now =
      let feChangeCreatedAt = now
          feCreatedAt = now
          feAuthor = ""
       in FirstEvent {..}
    toFirstEvent :: ELKChangeEvent -> FirstEvent -> FirstEvent
    toFirstEvent ELKChangeEvent {..} acc =
      let (createdAt, author) =
            -- If the event is older update the info
            if elkchangeeventCreatedAt < feCreatedAt acc
              then (elkchangeeventCreatedAt, authorMuid elkchangeeventAuthor)
              else (feCreatedAt acc, feAuthor acc)
       in FirstEvent
            { feChangeCreatedAt = elkchangeeventOnCreatedAt,
              feCreatedAt = createdAt,
              feAuthor = author
            }

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

-- | TopTerm agg query utils
getSimpleTR :: BH.TermsResult -> TermResult
getSimpleTR tr = TermResult (getTermKey tr) (BH.termsDocCount tr)

data TermResult = TermResult {trTerm :: Text, trCount :: Int} deriving (Show, Eq)

getTermKey :: BH.TermsResult -> Text
getTermKey (BH.TermsResult (BH.TextValue tv) _ _) = tv
getTermKey BH.TermsResult {} = error "Unexpected match"

getTermsAgg :: BH.Query -> Text -> Maybe Int -> TenantM [BH.TermsResult]
getTermsAgg query onTerm maxBuckets = do
  search <- aggSearch (Just query) aggs
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

newtype CountValue = CountValue {unCountValue :: Count}

instance FromJSON CountValue where
  parseJSON (Object v) = CountValue <$> v .: "value"
  parseJSON _ = mzero

getCardinalityAgg :: BH.FieldName -> Maybe Int -> QueryFlavor -> QueryM Count
getCardinalityAgg (BH.FieldName fieldName) threshold qf = do
  bhQuery <- getQueryBHWithFlavor qf

  let cardinality = Aeson.object ["field" .= fieldName, "precision_threshold" .= threshold]
      agg = Aeson.object ["agg1" .= Aeson.object ["cardinality" .= cardinality]]
      search = Aeson.object ["aggregations" .= agg, "size" .= (0 :: Word), "query" .= bhQuery]

  liftTenantM $ do
    index <- getIndexName
    res <- toAggRes <$> BHR.search index search
    pure $ unCountValue $ parseAggregationResults "agg1" res

countAuthors :: QueryFlavor -> QueryM Count
countAuthors = getCardinalityAgg (BH.FieldName "author.muid") (Just 3000)

getDocTypeTopCountByField :: NonEmpty ELKDocType -> Text -> Maybe Int -> QueryFlavor -> QueryM [TermResult]
getDocTypeTopCountByField doctype attr size qflavor = do
  -- Prepare the query
  basequery <- toBHQueryWithFlavor qflavor <$> getQuery
  let query = mkAnd $ basequery <> [documentTypes doctype]

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
    (ElkChange :| [])
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
    getRepoSummary fn = withFilter [mkTerm "repository_fullname" fn] $ do
      -- Prepare the queries
      let eventQF = QueryFlavor OnAuthor CreatedAt
          changeQF = QueryFlavor Author UpdatedAt

      -- Count the events
      totalChanges' <- withFilter [documentType ElkChangeCreatedEvent] (countDocs eventQF)
      openChanges' <- withFilter (changeState ElkChangeOpen) (countDocs changeQF)
      mergedChanges' <- withFilter [documentType ElkChangeMergedEvent] (countDocs eventQF)

      -- Return summary
      let abandonedChanges' = totalChanges' - (openChanges' + mergedChanges')
      pure $ RepoSummary fn totalChanges' abandonedChanges' mergedChanges' openChanges'

-- | get authors tops
getMostActiveAuthorByChangeCreated :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeCreated limit =
  getDocTypeTopCountByField
    (ElkChangeCreatedEvent :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostActiveAuthorByChangeMerged :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeMerged limit =
  getDocTypeTopCountByField
    (ElkChangeMergedEvent :| [])
    "on_author.muid"
    (Just limit)
    (QueryFlavor OnAuthor CreatedAt)

getMostActiveAuthorByChangeReviewed :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeReviewed limit =
  getDocTypeTopCountByField
    (ElkChangeReviewedEvent :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostActiveAuthorByChangeCommented :: Int -> QueryM [TermResult]
getMostActiveAuthorByChangeCommented limit =
  getDocTypeTopCountByField
    (ElkChangeCommentedEvent :| [])
    "author.muid"
    (Just limit)
    (QueryFlavor Author CreatedAt)

getMostReviewedAuthor :: Int -> QueryM [TermResult]
getMostReviewedAuthor limit =
  getDocTypeTopCountByField
    (ElkChangeReviewedEvent :| [])
    "on_author.muid"
    (Just limit)
    (QueryFlavor OnAuthor CreatedAt)

getMostCommentedAuthor :: Int -> QueryM [TermResult]
getMostCommentedAuthor limit =
  getDocTypeTopCountByField
    (ElkChangeCommentedEvent :| [])
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
    eventTypes :: NonEmpty ELKDocType
    eventTypes = fromList [ElkChangeReviewedEvent, ElkChangeCommentedEvent]
    qf = QueryFlavor Author CreatedAt
    getAuthorPeers :: Text -> QueryM (Text, [TermResult])
    getAuthorPeers peer = withFilter [mkTerm "author.muid" peer] $ do
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

data HistoInterval = Hour | Day | Week | Month | Year deriving (Eq, Show)

-- | Convert a duration to an interval that spans over maximum 24 buckets (31 for days)
durationToHistoInterval :: Pico -> HistoInterval
durationToHistoInterval sec
  | sec / day <= 1 = Hour
  | sec / month <= 1 = Day
  | sec / month <= 6 = Week
  | sec / year <= 2 = Month
  | otherwise = Year
  where
    year = month * 12
    month = day * 31
    day = 24 * 3600

histoIntervalToFormat :: HistoInterval -> Text
histoIntervalToFormat = \case
  Hour -> "yyyy-MM-dd HH:mm"
  Day -> "yyyy-MM-dd"
  Week -> "yyyy-MM-dd"
  Month -> "yyyy-MM"
  Year -> "yyyy"

calendarInterval :: HistoInterval -> Text
calendarInterval = \case
  Hour -> "hour"
  Day -> "day"
  Week -> "week"
  Month -> "month"
  Year -> "year"

dateInterval :: HistoInterval -> UTCTime -> Text
dateInterval hi = formatTime' formatStr
  where
    formatStr = case hi of
      Hour -> "%F %R"
      Day -> "%F"
      Week -> "%F"
      Month -> "%Y-%m"
      Year -> "%Y"

getNewContributors :: QueryM [TermResult]
getNewContributors = do
  -- Get query min bound
  (minDate, _) <- Q.queryBounds <$> getQuery

  let getDateLimit constraint =
        BH.QueryRangeQuery $
          BH.mkRangeQuery
            ( BH.FieldName (rangeField CreatedAt)
            )
            constraint

  let beforeBounceQ b = getDateLimit $ BH.RangeDateLt (BH.LessThanD b)
  let afterBounceQ b = getDateLimit $ BH.RangeDateGte (BH.GreaterThanEqD b)

  let runQ =
        getDocTypeTopCountByField
          (ElkChangeCreatedEvent :| [])
          "author.muid"
          (Just 5000)
          (QueryFlavor Author CreatedAt)

  -- Get author.muid term stats for ChangeCreatedEvent before and after bound
  beforeAuthor <- withFilter [beforeBounceQ minDate] runQ
  afterAuthor <- withFilter [afterBounceQ minDate] runQ

  -- Only keep after authors not present in the before authors list
  let ba = trTerm <$> beforeAuthor
  pure $ filter (\tr -> trTerm tr `notElem` ba) afterAuthor

-- | getReviewHisto
getHisto :: QueryFlavor -> QueryM (V.Vector HistoEventBucket)
getHisto qf = do
  query <- getQuery
  queryBH <- getQueryBHWithFlavor qf

  let (minDate, maxDate) = Q.queryBounds query
      duration = elapsedSeconds minDate maxDate
      interval = durationToHistoInterval duration

      bound =
        Aeson.object
          [ "min" .= dateInterval interval minDate,
            "max" .= dateInterval interval maxDate
          ]
      date_histo =
        Aeson.object
          [ "field" .= rangeField (qfRange qf),
            "calendar_interval" .= calendarInterval interval,
            "format" .= histoIntervalToFormat interval,
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

getHistoPB :: QueryFlavor -> QueryM (V.Vector SearchPB.Histo)
getHistoPB qf = fmap toPBHisto <$> getHisto qf
  where
    toPBHisto :: HistoEventBucket -> SearchPB.Histo
    toPBHisto HistoEventBucket {..} =
      let histoDate = heDate
          histoCount = heCount
       in SearchPB.Histo {..}

searchBody :: QueryFlavor -> Value -> QueryM Value
searchBody qf agg = do
  queryBH <- getQueryBHWithFlavor qf
  pure $
    Aeson.object
      [ "aggregations" .= Aeson.object ["agg1" .= agg],
        "size" .= (0 :: Word),
        "docvalue_fields"
          .= [ Aeson.object
                 [ "field" .= ("created_at" :: Text),
                   "format" .= ("date_time" :: Text)
                 ]
             ],
        "query" .= fromMaybe (error "need query") queryBH
      ]

queryAggValue :: Value -> QueryM Double
queryAggValue search = liftTenantM $ do
  index <- getIndexName
  res <- toAggRes <$> BHR.search index search
  pure $ getAggValue "agg1" res

averageDuration :: QueryFlavor -> QueryM Double
averageDuration qf = queryAggValue =<< searchBody qf avg
  where
    avg = Aeson.object ["avg" .= Aeson.object ["field" .= ("duration" :: Text)]]

medianDeviationDuration :: QueryFlavor -> QueryM Double
medianDeviationDuration qf = queryAggValue =<< searchBody qf deviation
  where
    deviation =
      Aeson.object
        [ "median_absolute_deviation"
            .= Aeson.object ["field" .= ("duration" :: Text)]
        ]

changeMergedAvgCommits :: QueryFlavor -> QueryM Double
changeMergedAvgCommits qf = queryAggValue =<< searchBody qf avg
  where
    avg = Aeson.object ["avg" .= Aeson.object ["field" .= ("commit_count" :: Text)]]

withDocTypes :: [ELKDocType] -> QueryM a -> QueryM a
withDocTypes docTypes = withFilter [mkOr $ toTermQuery <$> docTypes]
  where
    toTermQuery docType = mkTerm "type" (toText $ docTypeToText docType)

withDocType :: ELKDocType -> QueryM a -> QueryM a
withDocType docType = withDocTypes [docType]

-- | changes review stats
getReviewStats :: QueryM SearchPB.ReviewStats
getReviewStats = do
  reviewStatsCommentHisto <- getHisto' ElkChangeCommentedEvent
  reviewStatsReviewHisto <- getHisto' ElkChangeReviewedEvent

  commentCount <- withFilter [documentType ElkChangeCommentedEvent] statCount
  reviewCount <- withFilter [documentType ElkChangeReviewedEvent] statCount

  let reviewStatsCommentCount = Just commentCount
      reviewStatsReviewCount = Just reviewCount

  firstComments <- withFilter [documentType ElkChangeCommentedEvent] firstEventOnChanges
  firstReviews <- withFilter [documentType ElkChangeReviewedEvent] firstEventOnChanges

  let reviewStatsCommentDelay = firstEventAverageDuration firstComments
      reviewStatsReviewDelay = firstEventAverageDuration firstReviews

  pure $ SearchPB.ReviewStats {..}
  where
    qf = QueryFlavor Monocle.Search.Query.Author CreatedAt
    statCount :: QueryM SearchPB.ReviewCount
    statCount =
      SearchPB.ReviewCount
        <$> fmap countToWord (countAuthors qf)
        <*> fmap countToWord (countDocs qf)

    getHisto' docType = withDocType docType $ getHistoPB qf

-- | changes lifecycle stats
getLifecycleStats :: QueryM SearchPB.LifecycleStats
getLifecycleStats = do
  lifecycleStatsCreatedHisto <- getHisto' ElkChangeCreatedEvent
  lifecycleStatsUpdatedHisto <- getHistos' [ElkChangeCommitPushedEvent, ElkChangeCommitForcePushedEvent]
  lifecycleStatsMergedHisto <- getHisto' ElkChangeMergedEvent
  lifecycleStatsAbandonedHisto <- getHisto' ElkChangeAbandonedEvent

  (created, lifecycleStatsCreated) <- withDocType ElkChangeCreatedEvent $ do
    created <- countDocs qf
    stats <-
      SearchPB.ReviewCount
        <$> fmap countToWord (countAuthors qf)
        <*> pure (countToWord created)
    pure (created, Just stats)

  opened <- withFilter (changeState ElkChangeOpen) countDocs'
  merged <- withFilter (changeState ElkChangeMerged) countDocs'
  selfMerged' <- withFilter selfMerged countDocs'
  abandoned <- withFilter (changeState ElkChangeClosed) countDocs'

  let lifecycleStatsOpened = countToWord opened
      lifecycleStatsMerged = countToWord merged
      lifecycleStatsMergedRatio = merged `ratioF` created
      lifecycleStatsSelfMerged = countToWord selfMerged'
      lifecycleStatsSelfMergedRatio = selfMerged' `ratioF` created
      lifecycleStatsAbandoned = countToWord abandoned
      lifecycleStatsAbandonedRatio = abandoned `ratioF` created

  lifecycleStatsTtmMean <-
    double2Float
      <$> withFilter (changeState ElkChangeMerged) (averageDuration qf)
  lifecycleStatsTtmVariability <-
    double2Float
      <$> withFilter (changeState ElkChangeMerged) (medianDeviationDuration qf)

  updated <-
    withFilter
      [documentTypes $ fromList [ElkChangeCommitPushedEvent, ElkChangeCommitForcePushedEvent]]
      countEvents

  let lifecycleStatsUpdatesOfChanges = countToWord updated

  tests <- withFilter [documentType ElkChange, testIncluded] countDocs'
  let lifecycleStatsChangesWithTests = tests `ratioF` created
      lifecycleStatsIterationsPerChange = updated `ratioN` created

  lifecycleStatsCommitsPerChange <-
    double2Float
      <$> withFilter (changeState ElkChangeMerged) (changeMergedAvgCommits qf)

  pure $ SearchPB.LifecycleStats {..}
  where
    countEvents = countDocs (QueryFlavor Monocle.Search.Query.Author OnCreatedAt)
    qf = QueryFlavor Monocle.Search.Query.Author CreatedAt
    getHisto' docType = withDocType docType $ getHistoPB qf
    getHistos' docTypes = withDocTypes docTypes $ getHistoPB qf
    ratio :: Deci -> Count -> Count -> Deci
    ratio m x y
      | y == 0 = 0
      | otherwise = m * countToDeci x / countToDeci y
    ratioF x = fromFixed . ratio 100 x
    ratioN x = fromFixed . ratio 1 x

-- | authors activity stats
getAuthorHisto :: QueryFlavor -> QueryM (V.Vector (HistoBucket HistoAuthors))
getAuthorHisto qf = do
  query <- getQuery
  queryBH <- getQueryBHWithFlavor qf

  let (minDate, maxDate) = Q.queryBounds query
      duration = elapsedSeconds minDate maxDate
      interval = durationToHistoInterval duration

      bound =
        Aeson.object
          [ "min" .= dateInterval interval minDate,
            "max" .= dateInterval interval maxDate
          ]
      date_histo =
        Aeson.object
          [ "field" .= rangeField (qfRange qf),
            "calendar_interval" .= calendarInterval interval,
            "format" .= histoIntervalToFormat interval,
            "min_doc_count" .= (0 :: Word),
            "extended_bounds" .= bound
          ]
      author_agg =
        Aeson.object
          [ "authors"
              .= Aeson.object
                [ "terms"
                    .= Aeson.object
                      [ "field" .= ("author.muid" :: Text),
                        "size" .= (10000 :: Word)
                      ]
                ]
          ]
      agg =
        Aeson.object
          [ "agg1"
              .= Aeson.object
                [ "date_histogram" .= date_histo,
                  "aggs" .= author_agg
                ]
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
    let agg1 = parseAggregationResults "agg1" res
    pure $ hBuckets $ agg1

getActivityStats :: QueryM SearchPB.ActivityStats
getActivityStats = do
  changeCreatedHisto <- getHisto' ElkChangeCreatedEvent
  let activityStatsChangeAuthors = 0
      activityStatsCommentAuthors = 0
      activityStatsReviewAuthors = 0
      activityStatsCommentsHisto = fromList []
      activityStatsReviewsHisto = fromList []
      activityStatsChangesHisto = changeCreatedHisto
  pure $ SearchPB.ActivityStats {..}
  where
    qf = QueryFlavor Monocle.Search.Query.Author CreatedAt
    getHisto' docType = withDocType docType $ getHistoPB' qf
    getHistoPB' :: QueryFlavor -> QueryM (V.Vector SearchPB.Histo)
    getHistoPB' qf' = fmap toPBHisto <$> getAuthorHisto qf'
      where
        toPBHisto :: HistoBucket HistoAuthors -> SearchPB.Histo
        toPBHisto HistoBucket {..} =
          let histoDate = hbDate
              histoCount = fromInteger $ toInteger $ length $ haBuckets $ hbSubBuckets
           in SearchPB.Histo {..}
