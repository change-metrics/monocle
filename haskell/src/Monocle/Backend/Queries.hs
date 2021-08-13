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
import qualified Json.Extras as Json
import Monocle.Backend.Documents (ELKChange (..), ELKChangeEvent (..), ELKChangeState (..), ELKDocType (..), allEventTypes, authorMuid, changeStateToText, docTypeToText)
import Monocle.Env
import Monocle.Prelude hiding (doSearch)
import qualified Monocle.Search as SearchPB
import Monocle.Search.Query (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), rangeField)
import qualified Monocle.Search.Query as Q

-------------------------------------------------------------------------------
-- Low level wrappers for bloodhound. Only those should be using liftTenantM.

measureTenantM :: ToJSON body => body -> TenantM a -> QueryM a
measureTenantM body action = do
  prev <- liftIO getCurrentTime
  res <- liftTenantM action
  after <- liftIO getCurrentTime
  ctx <- fromMaybe "UNKNOWN" <$> getContext
  -- putTextLn $ ctx <> " " <> decodeUtf8 (encode body) <> " took " <> show (elapsedSeconds prev after)
  pure res

-- | Call the search endpoint
doSearchBH :: (ToJSON body, FromJSON resp) => body -> QueryM (BH.SearchResult resp)
doSearchBH body = do
  measureTenantM body $ do
    index <- getIndexName
    BHR.search index body

doSearchHitBH :: (ToJSON body) => body -> QueryM [Json.Value]
doSearchHitBH body = do
  measureTenantM body $ do
    index <- getIndexName
    BHR.searchHit index body

-- | Call the count endpoint
doCountBH :: BH.Query -> QueryM Count
doCountBH body = do
  measureTenantM body $ do
    index <- getIndexName
    resp <- BH.countByIndex index (BH.CountQuery body)
    case resp of
      Left e -> error $ show e
      Right x -> pure $ naturalToCount (BH.crCount x)

-------------------------------------------------------------------------------
-- Mid level queries

-- | Get search results hits
doSearch :: FromJSON resp => Maybe SearchPB.Order -> Word32 -> QueryM [resp]
doSearch orderM limit = do
  query <- getQueryBH
  resp <-
    doSearchBH
      (BH.mkSearch query Nothing)
        { BH.size = BH.Size $ fromInteger $ toInteger $ max 50 limit,
          BH.sortBody = toSortBody <$> orderM
        }
  pure $ catMaybes (fmap BH.hitSource <$> BH.hits . BH.searchHits $ resp)
  where
    toSortBody SearchPB.Order {..} =
      let field' =
            BH.FieldName
              . fromMaybe (error $ "Unknown sort field: " <> toStrict orderField)
              $ Q.queryFieldToDocument (toStrict orderField)
          order = sortOrder orderDirection
       in [ BH.DefaultSortSpec
              ( BH.DefaultSort field' order Nothing Nothing Nothing Nothing
              )
          ]
    sortOrder order = case fromPBEnum order of
      SearchPB.Order_DirectionASC -> BH.Ascending
      SearchPB.Order_DirectionDESC -> BH.Descending

-- | Get search results hits, as fast as possible
doFastSearch :: Word32 -> QueryM [Json.Value]
doFastSearch limit = do
  query <- getQueryBH
  doSearchHitBH
    (BH.mkSearch query Nothing)
      { BH.size = BH.Size $ fromInteger $ toInteger $ max 50 limit
      }

-- | Get document count matching the query
countDocs :: QueryM Count
countDocs = do
  query <-
    fromMaybe (error "Need a query to count") <$> getQueryBH
  doCountBH query

-- | Get aggregation results
doAggregation :: (ToJSON body) => body -> QueryM BH.AggregationResults
doAggregation body = toAggRes <$> doSearchBH body

toAggRes :: BH.SearchResult Value -> BH.AggregationResults
toAggRes res = fromMaybe (error "oops") (BH.aggregations res)

aggSearch :: Maybe BH.Query -> BH.Aggregations -> QueryM AggregationResultsWTH
aggSearch query aggs = do
  resp <- doSearchBH (BH.mkAggregateSearch query aggs)
  let totalHits = BH.value $ BH.hitsTotal $ BH.searchHits resp
  pure $ AggregationResultsWTH (toAggRes resp) totalHits

queryAggValue :: Value -> QueryM Double
queryAggValue search = getAggValue "agg1" <$> doAggregation search
  where
    getAggValue :: Text -> BH.AggregationResults -> Double
    getAggValue key = getValue . parseAggregationResults key

-- | Extract a single aggregation result from the map
parseAggregationResults :: (FromJSON a) => Text -> BH.AggregationResults -> a
parseAggregationResults key res = getExn $ do
  value <- Map.lookup key res `orDie` ("No value found for: " <> toString key)
  Aeson.parseEither Aeson.parseJSON value

queryAggResult :: FromJSON a => Value -> QueryM a
queryAggResult body = parseAggregationResults "agg1" <$> doAggregation body

-------------------------------------------------------------------------------
-- High level queries
changes :: Maybe SearchPB.Order -> Word32 -> QueryM [ELKChange]
changes orderM limit =
  withDocTypes [ElkChange] (QueryFlavor Author CreatedAt) $
    doSearch orderM limit

changeEvents :: LText -> Word32 -> QueryM (ELKChange, [ELKChangeEvent])
changeEvents changeID limit = dropQuery $
  withFilter [mkTerm "change_id" (toText changeID)] $ do
    change <- fromMaybe (error "Unknown change") . headMaybe <$> changes Nothing 1

    -- Collect all the events
    result <- withDocTypes allEventTypes (QueryFlavor Author CreatedAt) $ do
      doSearch Nothing limit

    pure (change, result)

-- | The change created / review ratio
changeReviewRatio :: QueryM Float
changeReviewRatio = withFlavor qf $ do
  commitCount <- withFilter [documentType ElkChangeCreatedEvent] $ countDocs
  reviewCount <-
    withFilter [documentTypes $ fromList [ElkChangeReviewedEvent, ElkChangeCommentedEvent]] $
      countDocs
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

data AggregationResultsWTH = AggregationResultsWTH
  { agResults :: BH.AggregationResults,
    agTH :: Int
  }
  deriving (Show, Eq)

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
    <$> withFilter (changeState ElkChangeOpen) countDocs
      <*> withFilter (changeState ElkChangeMerged) countDocs
      <*> withFilter (changeState ElkChangeClosed) countDocs
      <*> withFilter selfMergedQ countDocs
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

newtype HistoAuthors = HistoAuthors
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
  parseJSON (Object v) = do
    HistoBucket
      <$> v .: "key" <*> v .: "key_as_string" <*> v .: "doc_count" <*> parseSubBucket
    where
      subKeyName = bucketName (Proxy @a)
      parseSubBucket
        | subKeyName == "unused" = pure $ error "no subbucket"
        | otherwise = v .: subKeyName
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

data JsonChangeEvent = JsonChangeEvent
  { jceCreatedAt :: UTCTime,
    jceOnCreatedAt :: UTCTime,
    jceChangeId :: Json.ShortText,
    jceAuthor :: Json.ShortText
  }

decodeJsonChangeEvent :: Json.Value -> Maybe JsonChangeEvent
decodeJsonChangeEvent v = do
  jceCreatedAt <- Json.getDate =<< Json.getAttr "created_at" v
  jceOnCreatedAt <- Json.getDate =<< Json.getAttr "on_created_at" v
  jceChangeId <- Json.getString =<< Json.getAttr "change_id" v
  jceAuthor <- Json.getString =<< Json.getAttr "muid" =<< Json.getAttr "author" v
  pure $ JsonChangeEvent {..}

firstEventDuration :: FirstEvent -> Pico
firstEventDuration FirstEvent {..} = elapsedSeconds feChangeCreatedAt feCreatedAt

firstEventAverageDuration :: [FirstEvent] -> Word32
firstEventAverageDuration = truncate . fromMaybe 0 . average . map firstEventDuration

firstEventOnChanges :: QueryM [FirstEvent]
firstEventOnChanges = withFlavor (QueryFlavor Author CreatedAt) $ do
  (minDate, _) <- Q.queryBounds <$> getQuery

  -- Collect all the events
  resultJson <- doFastSearch 10000
  let result = catMaybes $ map decodeJsonChangeEvent resultJson

  -- Group by change_id
  let changeMap :: [NonEmpty JsonChangeEvent]
      changeMap = HM.elems $ groupBy jceChangeId result

  -- Remove old change where we may not have the first event
  let keepRecent :: NonEmpty JsonChangeEvent -> Bool
      keepRecent (JsonChangeEvent {..} :| _)
        | jceOnCreatedAt > minDate = True
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
    toFirstEvent :: JsonChangeEvent -> FirstEvent -> FirstEvent
    toFirstEvent JsonChangeEvent {..} acc =
      let (createdAt, author) =
            -- If the event is older update the info
            if jceCreatedAt < feCreatedAt acc
              then (jceCreatedAt, toLazy $ Json.toText jceAuthor)
              else (feCreatedAt acc, feAuthor acc)
       in FirstEvent
            { feChangeCreatedAt = jceOnCreatedAt,
              feCreatedAt = createdAt,
              feAuthor = author
            }

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

data TermResult = TermResult
  { trTerm :: Text,
    trCount :: Int
  }
  deriving (Show, Eq)

instance Ord TermResult where
  (TermResult _ x) `compare` (TermResult _ y) = x `compare` y

data TermsResultWTH = TermsResultWTH
  { tsrTR :: [TermResult],
    tsrTH :: Int
  }
  deriving (Show, Eq)

getTermKey :: BH.TermsResult -> Text
getTermKey (BH.TermsResult (BH.TextValue tv) _ _) = tv
getTermKey BH.TermsResult {} = error "Unexpected match"

getTermsAgg :: Maybe BH.Query -> Text -> Maybe Int -> QueryM TermsResultWTH
getTermsAgg query onTerm maxBuckets = do
  search <- aggSearch query aggs
  pure $
    TermsResultWTH
      (getSimpleTR <$> filter isNotEmptyTerm (unfilteredR $ agResults search))
      (agTH search)
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

getCardinalityAgg :: BH.FieldName -> Maybe Int -> QueryM Count
getCardinalityAgg (BH.FieldName fieldName) threshold = do
  bhQuery <- getQueryBH

  let cardinality = Aeson.object ["field" .= fieldName, "precision_threshold" .= threshold]
      agg = Aeson.object ["agg1" .= Aeson.object ["cardinality" .= cardinality]]
      search = Aeson.object ["aggregations" .= agg, "size" .= (0 :: Word), "query" .= bhQuery]
  unCountValue . parseAggregationResults "agg1" <$> doAggregation search

countAuthors :: QueryM Count
countAuthors = getCardinalityAgg (BH.FieldName "author.muid") (Just 3000)

getDocTypeTopCountByField ::
  NonEmpty ELKDocType -> Text -> Maybe Word32 -> QueryM TermsResultWTH
getDocTypeTopCountByField doctype attr size = withFilter [documentTypes doctype] $ do
  -- Prepare the query
  query <- getQueryBH
  runTermAgg query $ getSize size
  where
    runTermAgg query = getTermsAgg query attr
    getSize size' = toInt <$> size'
    toInt i =
      let i' = fromInteger $ toInteger i
       in if i' <= 0 then 10 else i'

-- | The repos_summary query
getRepos :: QueryM TermsResultWTH
getRepos =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (ElkChange :| []) "repository_fullname" (Just 5000)

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
  repos <- getRepos
  let names = trTerm <$> tsrTR repos
  traverse getRepoSummary names
  where
    getRepoSummary fn = withFilter [mkTerm "repository_fullname" fn] $ do
      -- Prepare the queries
      let eventQF = withFlavor (QueryFlavor OnAuthor CreatedAt)
          changeQF = withFlavor (QueryFlavor Author UpdatedAt)

      -- Count the events
      totalChanges' <- withFilter [documentType ElkChangeCreatedEvent] (eventQF countDocs)
      openChanges' <- withFilter (changeState ElkChangeOpen) (changeQF countDocs)
      mergedChanges' <- withFilter [documentType ElkChangeMergedEvent] (eventQF countDocs)

      -- Return summary
      let abandonedChanges' = totalChanges' - (openChanges' + mergedChanges')
      pure $ RepoSummary fn totalChanges' abandonedChanges' mergedChanges' openChanges'

-- | get authors tops
getMostActiveAuthorByChangeCreated :: Word32 -> QueryM TermsResultWTH
getMostActiveAuthorByChangeCreated limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (ElkChangeCreatedEvent :| []) "author.muid" (Just limit)

getMostActiveAuthorByChangeMerged :: Word32 -> QueryM TermsResultWTH
getMostActiveAuthorByChangeMerged limit =
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (ElkChangeMergedEvent :| []) "on_author.muid" (Just limit)

getMostActiveAuthorByChangeReviewed :: Word32 -> QueryM TermsResultWTH
getMostActiveAuthorByChangeReviewed limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (ElkChangeReviewedEvent :| []) "author.muid" (Just limit)

getMostActiveAuthorByChangeCommented :: Word32 -> QueryM TermsResultWTH
getMostActiveAuthorByChangeCommented limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (ElkChangeCommentedEvent :| []) "author.muid" (Just limit)

getMostReviewedAuthor :: Word32 -> QueryM TermsResultWTH
getMostReviewedAuthor limit =
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (ElkChangeReviewedEvent :| []) "on_author.muid" (Just limit)

getMostCommentedAuthor :: Word32 -> QueryM TermsResultWTH
getMostCommentedAuthor limit =
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (ElkChangeCommentedEvent :| []) "on_author.muid" (Just limit)

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
getAuthorsPeersStrength limit = withFlavor qf $ do
  peers <-
    getDocTypeTopCountByField
      eventTypes
      "author.muid"
      (Just 5000)
  authors_peers <- traverse (getAuthorPeers . trTerm) (tsrTR peers)
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
      pure (peer, tsrTR change_authors)
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
        withFlavor (QueryFlavor Author CreatedAt) $
          getDocTypeTopCountByField
            (ElkChangeCreatedEvent :| [])
            "author.muid"
            (Just 5000)

  -- Get author.muid term stats for ChangeCreatedEvent before and after bound
  beforeAuthor <- withModified Q.dropDate (withFilter [beforeBounceQ minDate] runQ)
  afterAuthor <- withModified Q.dropDate (withFilter [afterBounceQ minDate] runQ)

  -- Only keep after authors not present in the before authors list
  let ba = trTerm <$> tsrTR beforeAuthor
  pure $ filter (\tr -> trTerm tr `notElem` ba) (tsrTR afterAuthor)

-- | getChangesTop
getChangesTop :: Word32 -> Text -> QueryM TermsResultWTH
getChangesTop limit attr =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField
      (ElkChange :| [])
      attr
      -- Ask for a large amount of buckets to hopefully
      -- get a total count that is accurate
      (Just limit)

getChangesTopAuthors :: Word32 -> QueryM TermsResultWTH
getChangesTopAuthors limit = getChangesTop limit "author.muid"

getChangesTopRepos :: Word32 -> QueryM TermsResultWTH
getChangesTopRepos limit = getChangesTop limit "repository_fullname"

getChangesTopApprovals :: Word32 -> QueryM TermsResultWTH
getChangesTopApprovals limit = getChangesTop limit "approval"

getChangesTops :: Word32 -> QueryM SearchPB.ChangesTops
getChangesTops limit = do
  authors <- getChangesTopAuthors limit
  repos <- getChangesTopRepos limit
  approvals <- getChangesTopApprovals limit
  let result =
        let changesTopsAuthors = toTermsCount (toInt $ tsrTH authors) (tsrTR authors)
            changesTopsRepos = toTermsCount (toInt $ tsrTH repos) (tsrTR repos)
            changesTopsApprovals = toTermsCount (toInt $ tsrTH approvals) (tsrTR approvals)
         in SearchPB.ChangesTops {..}
  pure result
  where
    toPBTermCount TermResult {..} =
      SearchPB.TermCount
        (toLazy trTerm)
        (toInt trCount)
    toInt c = fromInteger $ toInteger c
    toTermsCount total tsc =
      Just $
        SearchPB.TermsCount
          { termsCountTermcount = V.fromList $ toPBTermCount <$> tsc,
            termsCountTotalHits = total
          }

-- | getReviewHisto
getHisto :: RangeFlavor -> QueryM (V.Vector HistoSimple)
getHisto rf = do
  query <- getQuery
  queryBH <- getQueryBH

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
          [ "field" .= rangeField rf,
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

  hBuckets . parseAggregationResults "agg1" <$> doAggregation search

getHistoPB :: RangeFlavor -> QueryM (V.Vector SearchPB.Histo)
getHistoPB rf = fmap toPBHisto <$> getHisto rf
  where
    toPBHisto :: HistoSimple -> SearchPB.Histo
    toPBHisto HistoBucket {..} =
      let histoDate = hbDate
          histoCount = hbCount
       in SearchPB.Histo {..}

searchBody :: QueryFlavor -> Value -> QueryM Value
searchBody qf agg = withFlavor qf $ do
  queryBH <- getQueryBH
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

withDocTypes :: [ELKDocType] -> QueryFlavor -> QueryM a -> QueryM a
withDocTypes docTypes flavor qm =
  withFilter [mkOr $ toTermQuery <$> docTypes] $ withFlavor flavor qm
  where
    toTermQuery docType = mkTerm "type" (toText $ docTypeToText docType)

withDocType :: ELKDocType -> QueryFlavor -> QueryM a -> QueryM a
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
        <$> fmap countToWord (withFlavor qf countAuthors)
        <*> fmap countToWord (withFlavor qf countDocs)

    getHisto' docType = withDocType docType qf (getHistoPB CreatedAt)

-- | changes lifecycle stats
getLifecycleStats :: QueryM SearchPB.LifecycleStats
getLifecycleStats = do
  lifecycleStatsCreatedHisto <- getHisto' ElkChangeCreatedEvent
  lifecycleStatsUpdatedHisto <- getHistos' [ElkChangeCommitPushedEvent, ElkChangeCommitForcePushedEvent]
  lifecycleStatsMergedHisto <- getHisto' ElkChangeMergedEvent
  lifecycleStatsAbandonedHisto <- getHisto' ElkChangeAbandonedEvent

  (created, lifecycleStatsCreated) <- withDocType ElkChangeCreatedEvent qf $ do
    created <- countDocs
    stats <-
      SearchPB.ReviewCount
        <$> fmap countToWord countAuthors
        <*> pure (countToWord created)
    pure (created, Just stats)

  opened <- withFilter (changeState ElkChangeOpen) countDocs
  merged <- withFilter (changeState ElkChangeMerged) countDocs
  selfMerged' <- withFilter selfMerged countDocs
  abandoned <- withFilter (changeState ElkChangeClosed) countDocs

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

  tests <- withFilter [documentType ElkChange, testIncluded] countDocs
  let lifecycleStatsChangesWithTests = tests `ratioF` created
      lifecycleStatsIterationsPerChange = updated `ratioN` created

  lifecycleStatsCommitsPerChange <-
    double2Float
      <$> withFilter (changeState ElkChangeMerged) (changeMergedAvgCommits qf)

  pure $ SearchPB.LifecycleStats {..}
  where
    countEvents = withFlavor (QueryFlavor Monocle.Search.Query.Author OnCreatedAt) countDocs
    qf = QueryFlavor Monocle.Search.Query.Author CreatedAt
    getHisto' docType = withDocType docType qf (getHistoPB CreatedAt)
    getHistos' docTypes = withDocTypes docTypes qf (getHistoPB CreatedAt)
    ratio :: Deci -> Count -> Count -> Deci
    ratio m x y
      | y == 0 = 0
      | otherwise = m * countToDeci x / countToDeci y
    ratioF x = fromFixed . ratio 100 x
    ratioN x = fromFixed . ratio 1 x

-- | authors activity stats
getAuthorHisto :: QueryFlavor -> QueryM (V.Vector (HistoBucket HistoAuthors))
getAuthorHisto qf = withFlavor qf $ do
  query <- getQuery
  queryBH <- getQueryBH

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

  hBuckets . parseAggregationResults "agg1" <$> doAggregation search

getActivityStats :: QueryM SearchPB.ActivityStats
getActivityStats = do
  changeCreatedHisto <- getHisto' ElkChangeCreatedEvent
  changeCommentedHisto <- getHisto' ElkChangeCommentedEvent
  changeReviewedHisto <- getHisto' ElkChangeReviewedEvent

  changeAuthorsCount <- runCount ElkChangeCreatedEvent
  commentAuthorsCount <- runCount ElkChangeCommentedEvent
  reviewAuthorsCount <- runCount ElkChangeReviewedEvent

  let activityStatsChangeAuthors = changeAuthorsCount
      activityStatsCommentAuthors = commentAuthorsCount
      activityStatsReviewAuthors = reviewAuthorsCount
      activityStatsCommentsHisto = changeCommentedHisto
      activityStatsReviewsHisto = changeReviewedHisto
      activityStatsChangesHisto = changeCreatedHisto
  pure $ SearchPB.ActivityStats {..}
  where
    qf = QueryFlavor Author CreatedAt
    runCount docType = countToWord <$> withDocType docType qf countAuthors
    getHisto' docType = withDocType docType qf getHistoPB'
    getHistoPB' :: QueryM (V.Vector SearchPB.Histo)
    getHistoPB' = fmap toPBHisto <$> getAuthorHisto qf
    toPBHisto :: HistoBucket HistoAuthors -> SearchPB.Histo
    toPBHisto HistoBucket {..} =
      let histoDate = hbDate
          histoCount =
            fromInteger
              . toInteger
              . length
              $ haBuckets hbSubBuckets
       in SearchPB.Histo {..}
