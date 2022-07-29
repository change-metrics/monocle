-- | Monocle queries
-- The goal of this module is to transform 'Query' into list of items
module Monocle.Backend.Queries where

import Data.Aeson (Value (Object), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.HashMap.Strict qualified as HM
import Data.List qualified
import Data.Map qualified as Map
import Data.Time (UTCTime (UTCTime), addDays, addGregorianMonthsClip, addGregorianYearsClip, secondsToNominalDiffTime)
import Data.Time.Clock (secondsToDiffTime)
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Database.Bloodhound.Raw (TermsCompositeAggBucket)
import Database.Bloodhound.Raw qualified as BHR
import Json.Extras qualified as Json
import Monocle.Backend.Documents (EChange (..), EChangeEvent (..), EChangeState (..), EDocType (..), allEventTypes)
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Prelude hiding (doSearch)
import Monocle.Protob.Metric qualified as MetricPB
import Monocle.Protob.Search qualified as SearchPB
import Monocle.Search.Query (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), rangeField)
import Monocle.Search.Query qualified as Q
import Streaming.Prelude qualified as Streaming

-------------------------------------------------------------------------------
-- Low level wrappers for bloodhound.
measureQueryM :: (QueryMonad m, ToJSON body) => body -> m a -> m a
measureQueryM body action = do
  prev <- getCurrentTime'
  res <- action
  after <- getCurrentTime'
  ctxM <- getContext
  case ctxM of
    Just ctx ->
      trace' $ ctx <> " " <> decodeUtf8 (encode body) <> " took " <> show (elapsedSeconds prev after)
    Nothing -> pure ()
  pure res

-- | Call the search endpoint
doScrollSearchBH :: (QueryMonad m, ToJSON body, FromJSONField resp) => BHR.ScrollRequest -> body -> m (BH.SearchResult resp)
doScrollSearchBH scrollRequest body = do
  measureQueryM body $ do
    index <- getIndexName
    elasticSearch index body scrollRequest

-- | A search without scroll
doSearchBH :: (QueryMonad m, ToJSON body, FromJSONField resp) => body -> m (BH.SearchResult resp)
doSearchBH = doScrollSearchBH BHR.NoScroll

doAdvanceScrollBH :: (QueryMonad m, FromJSON resp) => BH.ScrollId -> m (BH.SearchResult resp)
doAdvanceScrollBH scroll = do
  measureQueryM (Aeson.object ["scrolling" .= ("advancing..." :: Text)]) $ do
    elasticAdvance scroll

doSearchHitBH :: (QueryMonad m, ToJSON body) => body -> m [Json.Value]
doSearchHitBH body = do
  measureQueryM body $ do
    index <- getIndexName
    elasticSearchHit index body

-- | Call the count endpoint
doCountBH :: QueryMonad m => BH.Query -> m Count
doCountBH body = do
  measureQueryM body $ do
    index <- getIndexName
    resp <- elasticCountByIndex index (BH.CountQuery body)
    case resp of
      Left e -> error $ show e
      Right x -> pure $ naturalToCount (BH.crCount x)

-- | Call _delete_by_query endpoint
doDeleteByQueryBH :: (QueryMonad m, BH.MonadBH m) => BH.Query -> m ()
doDeleteByQueryBH body = do
  measureQueryM body $ do
    index <- getIndexName
    -- TODO: BH does not return parsed response - keep as is or if not enough move it to BHR.
    void $ elasticDeleteByQuery index body
    void $ BH.refreshIndex index

-------------------------------------------------------------------------------
-- Mid level queries

-- | scan search the result using a streaming
scanSearch :: FromJSONField resp => Stream (Of (BH.Hit resp)) QueryM ()
scanSearch = do
  resp <- lift $ do
    query <- getQueryBH
    let search = (BH.mkSearch query Nothing) {BH.size = BH.Size 5000}
    doScrollSearchBH (BHR.GetScroll "1m") search
  go (getHits resp) (BH.scrollId resp)
 where
  -- no more result, stop here
  go [] _ = pure ()
  -- no more scroll, yield the result and stop
  go xs Nothing = Streaming.each xs
  -- otherwise, keep on scrolling
  go xs (Just sc) = do
    Streaming.each xs
    resp <- lift $ doAdvanceScrollBH sc
    go (getHits resp) (BH.scrollId resp)

  -- helper to get the hits of a search result
  getHits = BH.hits . BH.searchHits

-- | scan search the hit body, see the 'concat' doc for why we don't need catMaybes
-- https://hackage.haskell.org/package/streaming-0.2.3.0/docs/Streaming-Prelude.html#v:concat
scanSearchHit :: FromJSONField resp => Stream (Of resp) QueryM ()
scanSearchHit = Streaming.concat $ Streaming.map BH.hitSource scanSearch

-- | scan search the document id, here is an example usage for the REPL:
-- λ> testQueryM (defaultTenant "zuul") $ runQueryM (mkQuery []) $ Streaming.print scanSearchId
-- DocId ...
-- DocId ...
scanSearchId :: Stream (Of BH.DocId) QueryM ()
scanSearchId = Streaming.map BH.hitDocId anyScan
 where
  -- here we need to help ghc figures out what fromJSON to use
  anyScan :: Stream (Of (BH.Hit AnyJSON)) QueryM ()
  anyScan = scanSearch

scanSearchSimple :: FromJSONField resp => QueryM [resp]
scanSearchSimple = Streaming.toList_ scanSearchHit

-- | Get search results hits
doSearch :: QueryMonad m => FromJSONField resp => Maybe SearchPB.Order -> Word32 -> m [resp]
doSearch orderM limit = do
  query <- getQueryBH
  resp <-
    doSearchBH
      (BH.mkSearch query Nothing)
        { BH.size = BH.Size $ fromInteger $ toInteger $ max 50 limit
        , BH.sortBody = toSortBody <$> orderM
        }
  pure $ catMaybes (fmap BH.hitSource <$> BH.hits . BH.searchHits $ resp)
 where
  toSortBody SearchPB.Order {..} =
    let field' =
          BH.FieldName
            . fromMaybe (error $ "Unknown sort field: " <> from orderField)
            $ Q.queryFieldToDocument (from orderField)
        order = sortOrder orderDirection
     in [ BH.DefaultSortSpec
            ( BH.DefaultSort field' order Nothing Nothing Nothing Nothing
            )
        ]
  sortOrder order = case fromPBEnum order of
    SearchPB.Order_DirectionASC -> BH.Ascending
    SearchPB.Order_DirectionDESC -> BH.Descending

-- | Get search results hits, as fast as possible
doFastSearch :: QueryMonad m => Word32 -> m [Json.Value]
doFastSearch limit = do
  query <- getQueryBH
  doSearchHitBH
    (BH.mkSearch query Nothing)
      { BH.size = BH.Size $ fromInteger $ toInteger $ max 50 limit
      }

-- | Get document count matching the query
countDocs :: QueryMonad m => m Count
countDocs = do
  query <-
    fromMaybe (error "Need a query to count") <$> getQueryBH
  doCountBH query

-- | Delete documents matching the query
deleteDocs :: (QueryMonad m, BH.MonadBH m) => m ()
deleteDocs = do
  query <-
    fromMaybe (error "Need a query to delete") <$> getQueryBH
  void $ doDeleteByQueryBH query

-- | Get aggregation results
doAggregation :: QueryMonad m => (ToJSON body) => body -> m BH.AggregationResults
doAggregation body = toAggRes <$> doSearchBH body

toAggRes :: BH.SearchResult Value -> BH.AggregationResults
toAggRes res = fromMaybe (error "oops") (BH.aggregations res)

aggSearch :: QueryMonad m => Maybe BH.Query -> BH.Aggregations -> m AggregationResultsWTH
aggSearch query aggs = do
  resp <- doSearchBH (BH.mkAggregateSearch query aggs)
  let totalHits = BH.value $ BH.hitsTotal $ BH.searchHits resp
  pure $ AggregationResultsWTH (toAggRes resp) totalHits

queryAggValue :: QueryMonad m => Value -> m Double
queryAggValue search = getAggValue "agg1" <$> doAggregation search
 where
  getAggValue :: Text -> BH.AggregationResults -> Double
  getAggValue key = getValue . parseAggregationResults key

-- | Extract a single aggregation result from the map
parseAggregationResults :: (FromJSON a) => Text -> BH.AggregationResults -> a
parseAggregationResults key res = getExn $ do
  value <- Map.lookup (from key) res `orDie` ("No value found for: " <> from key)
  Aeson.parseEither Aeson.parseJSON value

queryAggResult :: QueryMonad m => FromJSON a => Value -> m a
queryAggResult body = parseAggregationResults "agg1" <$> doAggregation body

-- | Run a Terms composite aggregation (composite agg result is paginated)
-- | Composite aggregations are adviced when dealing with high cardinality terms
doTermsCompositeAgg :: forall m. QueryMonad m => Text -> Stream (Of BHR.TermsCompositeAggBucket) m ()
doTermsCompositeAgg term = getPages Nothing
 where
  getPages :: Maybe Value -> Stream (Of BHR.TermsCompositeAggBucket) m ()
  getPages afterM = do
    ret <- lift $ queryAggResult $ BHR.mkAgg [BHR.mkTermsCompositeAgg term afterM] Nothing Nothing
    Streaming.each $ getBuckets ret
    case getAfterValue ret of
      Just afterValue -> getPages (Just afterValue)
      Nothing -> pure ()
  getAfterValue :: BHR.TermsCompositeAggResult -> Maybe Value
  getAfterValue (BHR.TermsCompositeAggResult (Just (BHR.TermsCompositeAggKey v)) _) = Just v
  getAfterValue (BHR.TermsCompositeAggResult Nothing _) = Nothing
  getBuckets :: BHR.TermsCompositeAggResult -> V.Vector BHR.TermsCompositeAggBucket
  getBuckets (BHR.TermsCompositeAggResult _ buckets) = buckets

-------------------------------------------------------------------------------
-- High level queries
changes :: QueryMonad m => Maybe SearchPB.Order -> Word32 -> m [EChange]
changes orderM limit =
  withDocTypes [EChangeDoc] (QueryFlavor Author UpdatedAt) $
    doSearch orderM limit

changeEvents :: QueryMonad m => LText -> Word32 -> m (EChange, [EChangeEvent])
changeEvents changeID limit = dropQuery $
  withFilter [mkTerm "change_id" (from changeID)] $ do
    change <- fromMaybe (error "Unknown change") . headMaybe <$> changes Nothing 1

    -- Collect all the events
    result <- withDocTypes allEventTypes (QueryFlavor Author CreatedAt) $ do
      doSearch Nothing limit

    pure (change, result)

data RatioQuery = CHANGES_VS_REVIEWS_RATIO | COMMITS_VS_REVIEWS_RATIO

getRatio :: QueryMonad m => RatioQuery -> m Float
getRatio = \case
  CHANGES_VS_REVIEWS_RATIO -> changesReviewsRatio
  COMMITS_VS_REVIEWS_RATIO -> commitsReviewsRatio

-- | The base review ratio
baseReviewsRatio :: QueryMonad m => [EDocType] -> m Float
baseReviewsRatio events = withFlavor qf $ do
  count <- withFilter [documentTypes $ fromList events] countDocs
  reviewCount <-
    withFilter
      [documentTypes $ fromList [EChangeReviewedEvent, EChangeCommentedEvent]]
      countDocs
  let total, countF, reviewCountF :: Float
      total = reviewCountF + countF
      reviewCountF = fromIntegral reviewCount
      countF = fromIntegral count
  pure (if total > 0 then reviewCountF * 100 / total else -1)
 where
  -- Author makes query author match the change event author, not the receiver of the event.
  -- CreatedAt is necessary for change event.
  qf = QueryFlavor Author CreatedAt

-- | The changes created / reviews ratio
changesReviewsRatio :: QueryMonad m => m Float
changesReviewsRatio = baseReviewsRatio [EChangeCreatedEvent]

-- | The commits / reviews ratio
commitsReviewsRatio :: QueryMonad m => m Float
commitsReviewsRatio =
  baseReviewsRatio
    [ EChangeCommitForcePushedEvent
    , EChangeCommitPushedEvent
    ]

-- | Scroll over all know authors
getAllAuthorsMuid :: QueryMonad m => Stream (Of Text) m ()
getAllAuthorsMuid = do
  Streaming.mapMaybe trans $ local updateEnv (doTermsCompositeAgg "author.muid")
 where
  trans :: TermsCompositeAggBucket -> Maybe Text
  trans (BHR.TermsCompositeAggBucket (BHR.TermsCompositeAggKey value) _) = case value of
    Aeson.String muid -> Just muid
    _ -> Nothing
  updateEnv = addFilter [documentTypes $ fromList allEventTypes]

getAllAuthorsMuid' :: QueryMonad m => m [Text]
getAllAuthorsMuid' = Streaming.toList_ getAllAuthorsMuid

-- | Add a change state filter to the query
changeState :: EChangeState -> [BH.Query]
changeState state' =
  [ BH.TermQuery (BH.Term "type" "Change") Nothing
  , BH.TermQuery (BH.Term "state" $ from state') Nothing
  ]

-- | Add a document type filter to the query
documentTypes :: NonEmpty EDocType -> BH.Query
documentTypes doc = BH.TermsQuery "type" $ from <$> doc

documentType :: EDocType -> BH.Query
documentType x = documentTypes (x :| [])

-- | User query
toUserTerm :: Text -> BH.Query
toUserTerm user = BH.TermQuery (BH.Term "author.muid" user) Nothing

data AggregationResultsWTH = AggregationResultsWTH
  { agResults :: BH.AggregationResults
  , agTH :: Int
  }
  deriving (Show, Eq)

-- | Event counts
data EventCounts = EventCounts
  { openedCount :: Count
  , mergedCount :: Count
  , abandonedCount :: Count
  , selfMergedCount :: Count
  }
  deriving (Eq, Show)

-- | Author histo
data HistoAuthorBucket = HistoAuthorBucket
  { habKey :: LText
  , habCount :: Word32
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
  { hbKey :: Word64
  , hbDate :: LText
  , hbCount :: Word32
  , hbSubBuckets :: Maybe a
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
      <$> v .: "key"
      <*> v .: "key_as_string"
      <*> v .: "doc_count"
      <*> parseSubBucket
   where
    subKeyName = bucketName (Proxy @a)
    parseSubBucket
      | subKeyName == "unused" = pure Nothing
      | otherwise = v .:? from subKeyName
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
  { avg :: Double
  , variability :: Double
  }
  deriving (Eq, Show)

field :: Text -> Value
field name = Aeson.object ["field" .= name]

data FirstEvent = FirstEvent
  { feChangeCreatedAt :: UTCTime
  , feCreatedAt :: UTCTime
  , feAuthor :: LText
  }
  deriving (Show)

data JsonChangeEvent = JsonChangeEvent
  { jceCreatedAt :: UTCTime
  , jceOnCreatedAt :: UTCTime
  , jceChangeId :: Json.ShortText
  , jceAuthor :: Json.ShortText
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

firstEventOnChanges :: QueryMonad m => m [FirstEvent]
firstEventOnChanges = do
  (minDate, _) <- Q.queryBounds <$> getQuery

  -- Collect all the events
  resultJson <- doFastSearch 10000
  let result = mapMaybe decodeJsonChangeEvent resultJson

  -- Group by change_id
  let changeMap :: [NonEmpty JsonChangeEvent]
      changeMap = HM.elems $ groupBy jceChangeId result

  -- Remove old change where we may not have the first event
  let keepRecent :: NonEmpty JsonChangeEvent -> Bool
      keepRecent (JsonChangeEvent {..} :| _)
        | jceOnCreatedAt > minDate = True
        | otherwise = False

  now <- getCurrentTime'

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
            then (jceCreatedAt, from $ Json.toText jceAuthor)
            else (feCreatedAt acc, feAuthor acc)
     in FirstEvent
          { feChangeCreatedAt = jceOnCreatedAt
          , feCreatedAt = createdAt
          , feAuthor = author
          }

-- | The achievement query
data ProjectBucket = ProjectBucket
  { pbKey :: LText
  , pbCount :: Word32
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
  { epbType :: LText
  , epbCount :: Word32
  , epbProjects :: [ProjectBucket]
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

getProjectAgg :: QueryMonad m => BH.Query -> m [EventProjectBucketAgg]
getProjectAgg query = do
  -- TODO: check why this is not calling the low-level function defined in this module
  res <- toAggRes <$> doSearchBH (BHR.aggWithDocValues agg (Just query))
  pure $ unEPBuckets (parseAggregationResults "agg" res)
 where
  agg =
    [
      ( "agg"
      , Aeson.object
          [ "terms" .= field "type"
          , "aggs"
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
  { trTerm :: Text
  , trCount :: Int
  }
  deriving (Show, Eq)

instance Ord TermResult where
  (TermResult _ x) `compare` (TermResult _ y) = x `compare` y

data TermsResultWTH = TermsResultWTH
  { tsrTR :: [TermResult]
  , tsrTH :: Int
  }
  deriving (Show, Eq)

getTermKey :: BH.TermsResult -> Text
getTermKey (BH.TermsResult (BH.TextValue tv) _ _) = tv
getTermKey BH.TermsResult {} = error "Unexpected match"

getTermsAgg :: QueryMonad m => Maybe BH.Query -> Text -> Maybe Int -> m TermsResultWTH
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

getCardinalityAgg :: QueryMonad m => BH.FieldName -> Maybe Int -> m Count
getCardinalityAgg (BH.FieldName fieldName) threshold = do
  bhQuery <- getQueryBH

  let cardinality = Aeson.object ["field" .= fieldName, "precision_threshold" .= threshold]
      agg = Aeson.object ["agg1" .= Aeson.object ["cardinality" .= cardinality]]
      search = Aeson.object ["aggregations" .= agg, "size" .= (0 :: Word), "query" .= bhQuery]
  unCountValue . parseAggregationResults "agg1" <$> doAggregation search

countAuthors :: QueryMonad m => m Count
countAuthors = getCardinalityAgg (BH.FieldName "author.muid") (Just 3000)

getDocTypeTopCountByField :: QueryMonad m => NonEmpty EDocType -> Text -> Maybe Word32 -> m TermsResultWTH
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

openChangesCount :: QueryMonad m => m Count
openChangesCount = withFilter (changeState EChangeOpen) (withoutDate countDocs)
 where
  withoutDate = withModified Q.dropDate

-- | The repos_summary query
getRepos :: QueryMonad m => m TermsResultWTH
getRepos =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeDoc :| []) "repository_fullname" (Just 5000)

data RepoSummary = RepoSummary
  { fullname :: Text
  , createdChanges :: Count
  , abandonedChanges :: Count
  , mergedChanges :: Count
  , updatedChanges :: Count
  , openChanges :: Count
  }
  deriving (Show, Eq)

getReposSummary :: QueryMonad m => m [RepoSummary]
getReposSummary = do
  repos <- getRepos
  let names = trTerm <$> tsrTR repos
  traverse getRepoSummary names
 where
  withoutRepoFilters =
    -- Remove the initial repo filter to speedup the summary query.
    -- It is not necessary to keep such filter as we already got the repos list.
    -- This is important for project with huge list of repository regex.
    withModified (Q.dropField (`elem` ["repo", "project"]))
  withRepo fn = withoutRepoFilters . withFilter [mkTerm "repository_fullname" fn]

  getRepoSummary fullname = withRepo fullname $ do
    -- Prepare the queries
    let changeQF = withFlavor (QueryFlavor Author UpdatedAt)

    -- Count the events
    createdChanges <- wordToCount <$> runMetricNum metricChangesCreated
    updatedChanges <- withFilter (changeState EChangeOpen) (changeQF countDocs)
    mergedChanges <- wordToCount <$> runMetricNum metricChangesMerged
    openChanges <- openChangesCount
    abandonedChanges <- wordToCount <$> runMetricNum metricChangesAbandoned

    pure $ RepoSummary {..}

-- | get authors tops
getMostActiveAuthorByChangeCreated :: QueryMonad m => Word32 -> m TermsResultWTH
getMostActiveAuthorByChangeCreated limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeCreatedEvent :| []) "author.muid" (Just limit)

getMostActiveAuthorByChangeMerged :: QueryMonad m => Word32 -> m TermsResultWTH
getMostActiveAuthorByChangeMerged limit =
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (EChangeMergedEvent :| []) "on_author.muid" (Just limit)

getMostActiveAuthorByChangeReviewed :: QueryMonad m => Word32 -> m TermsResultWTH
getMostActiveAuthorByChangeReviewed limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeReviewedEvent :| []) "author.muid" (Just limit)

getMostActiveAuthorByChangeCommented :: QueryMonad m => Word32 -> m TermsResultWTH
getMostActiveAuthorByChangeCommented limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeCommentedEvent :| []) "author.muid" (Just limit)

getMostReviewedAuthor :: QueryMonad m => Word32 -> m TermsResultWTH
getMostReviewedAuthor limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeReviewedEvent :| []) "on_author.muid" (Just limit)

getMostCommentedAuthor :: QueryMonad m => Word32 -> m TermsResultWTH
getMostCommentedAuthor limit =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeCommentedEvent :| []) "on_author.muid" (Just limit)

-- | peer strength authors
data PeerStrengthResult = PeerStrengthResult
  { psrAuthor :: Text
  , psrPeer :: Text
  , psrStrength :: Word32
  }
  deriving (Show, Eq)

instance Ord PeerStrengthResult where
  (PeerStrengthResult _ _ x) `compare` (PeerStrengthResult _ _ y) =
    x `compare` y

getAuthorsPeersStrength :: QueryMonad m => Word32 -> m [PeerStrengthResult]
getAuthorsPeersStrength limit = withFlavor qf $ do
  peers <-
    getDocTypeTopCountByField
      eventTypes
      "author.muid"
      (Just limit)
  authors_peers <- traverse (getAuthorPeers . trTerm) (tsrTR peers)
  pure $
    take (fromInteger $ toInteger limit) $
      reverse $
        sort $
          filter (\psr -> psrAuthor psr /= psrPeer psr) $
            concatMap transform authors_peers
 where
  eventTypes :: NonEmpty EDocType
  eventTypes = fromList [EChangeReviewedEvent, EChangeCommentedEvent]
  qf = QueryFlavor Author CreatedAt
  getAuthorPeers :: QueryMonad m => Text -> m (Text, [TermResult])
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

-- | Convert a duration to an interval that spans over maximum 24 buckets (31 for days)
newtype TimeFormat = TimeFormat {getFormat :: Text}

instance From Q.TimeRange TimeFormat where
  from hi = TimeFormat $ case hi of
    Q.Hour -> "yyyy-MM-dd HH:mm"
    Q.Day -> "yyyy-MM-dd"
    Q.Week -> "yyyy-MM-dd"
    Q.Month -> "yyyy-MM"
    Q.Year -> "yyyy"

dateInterval :: Q.TimeRange -> UTCTime -> Text
dateInterval hi = formatTime' formatStr
 where
  formatStr = case hi of
    Q.Hour -> "%F %R"
    Q.Day -> "%F"
    Q.Week -> "%F"
    Q.Month -> "%Y-%m"
    Q.Year -> "%Y"

getNewContributors :: QueryMonad m => m [TermResult]
getNewContributors = do
  -- Get query min bound
  (minDate, _) <- Q.queryBounds <$> getQuery

  let getDateLimit constraint =
        BH.QueryRangeQuery $
          BH.mkRangeQuery (BH.FieldName "created_at") constraint

  let beforeBounceQ b = getDateLimit $ BH.RangeDateLt (BH.LessThanD b)
  let afterBounceQ b = getDateLimit $ BH.RangeDateGte (BH.GreaterThanEqD b)

  let runQ =
        withFlavor (QueryFlavor Author CreatedAt) $
          getDocTypeTopCountByField
            (EChangeCreatedEvent :| [])
            "author.muid"
            (Just 5000)

  -- Get author.muid term stats for ChangeCreatedEvent before and after bound
  beforeAuthor <- withModified Q.dropDate (withFilter [beforeBounceQ minDate] runQ)
  afterAuthor <- withModified Q.dropDate (withFilter [afterBounceQ minDate] runQ)

  -- Only keep after authors not present in the before authors list
  let ba = trTerm <$> tsrTR beforeAuthor
  pure $ filter (\tr -> trTerm tr `notElem` ba) (tsrTR afterAuthor)

-- | getChangesTop
getChangesTop :: QueryMonad m => Word32 -> Text -> m TermsResultWTH
getChangesTop limit attr =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField
      (EChangeDoc :| [])
      attr
      -- Ask for a large amount of buckets to hopefully
      -- get a total count that is accurate
      (Just limit)

getChangesTopAuthors :: QueryMonad m => Word32 -> m TermsResultWTH
getChangesTopAuthors limit = getChangesTop limit "author.muid"

getChangesTopRepos :: QueryMonad m => Word32 -> m TermsResultWTH
getChangesTopRepos limit = getChangesTop limit "repository_fullname"

getChangesTopApprovals :: QueryMonad m => Word32 -> m TermsResultWTH
getChangesTopApprovals limit = getChangesTop limit "approval"

getChangesTops :: QueryMonad m => Word32 -> m SearchPB.ChangesTops
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
    MetricPB.TermCountInt
      (from trTerm)
      (toInt trCount)
  toInt c = fromInteger $ toInteger c
  toTermsCount total tsc =
    Just $
      MetricPB.TermsCountInt
        { termsCountIntTermcount = V.fromList $ toPBTermCount <$> tsc
        , termsCountIntTotalHits = total
        }

searchBody :: QueryMonad m => QueryFlavor -> Value -> m Value
searchBody qf agg = withFlavor qf $ do
  queryBH <- getQueryBH
  pure $
    Aeson.object
      [ "aggregations" .= Aeson.object ["agg1" .= agg]
      , "size" .= (0 :: Word)
      , "docvalue_fields"
          .= [ Aeson.object
                [ "field" .= ("created_at" :: Text)
                , "format" .= ("date_time" :: Text)
                ]
             ]
      , "query" .= fromMaybe (error "need query") queryBH
      ]

averageDuration :: QueryMonad m => QueryFlavor -> m Double
averageDuration qf = queryAggValue =<< searchBody qf avg
 where
  avg = Aeson.object ["avg" .= Aeson.object ["field" .= ("duration" :: Text)]]

medianDeviationDuration :: QueryMonad m => QueryFlavor -> m Double
medianDeviationDuration qf = queryAggValue =<< searchBody qf deviation
 where
  deviation =
    Aeson.object
      [ "median_absolute_deviation"
          .= Aeson.object ["field" .= ("duration" :: Text)]
      ]

changeMergedAvgCommits :: QueryMonad m => QueryFlavor -> m Double
changeMergedAvgCommits qf = queryAggValue =<< searchBody qf avg
 where
  avg = Aeson.object ["avg" .= Aeson.object ["field" .= ("commit_count" :: Text)]]

withDocTypes :: QueryMonad m => [EDocType] -> QueryFlavor -> m a -> m a
withDocTypes docTypes flavor qm =
  withFilter [mkOr $ toTermQuery <$> docTypes] $ withFlavor flavor qm
 where
  toTermQuery docType = mkTerm "type" (from docType)

withDocType :: QueryMonad m => EDocType -> QueryFlavor -> m a -> m a
withDocType docType = withDocTypes [docType]

withEvents :: QueryMonad m => [BH.Query] -> m a -> m a
withEvents ev = withFlavor (QueryFlavor Author OnCreatedAndCreated) . withFilter ev

-- | changes review stats
getReviewStats :: QueryMonad m => m SearchPB.ReviewStats
getReviewStats = do
  reviewStatsCommentHisto <- runMetricTrendIntPB metricComments
  reviewStatsReviewHisto <- runMetricTrendIntPB metricReviews

  reviewStatsCommentCount <- Just <$> statCountC
  reviewStatsReviewCount <- Just <$> statCountR

  reviewStatsCommentDelay <- runMetricNum metricFirstCommentMeanTime
  reviewStatsReviewDelay <- runMetricNum metricFirstReviewMeanTime

  pure $ SearchPB.ReviewStats {..}
 where
  statCountR =
    SearchPB.ReviewCount
      <$> runMetricNum metricReviewAuthors
      <*> runMetricNum metricReviews
  statCountC =
    SearchPB.ReviewCount
      <$> runMetricNum metricCommentAuthors
      <*> runMetricNum metricComments

-- | changes lifecycle stats
getLifecycleStats :: QueryMonad m => m SearchPB.LifecycleStats
getLifecycleStats = do
  lifecycleStatsCreatedHisto <- runMetricTrendIntPB metricChangesCreated
  lifecycleStatsUpdatedHisto <- runMetricTrendIntPB metricChangeAuthors
  lifecycleStatsMergedHisto <- runMetricTrendIntPB metricChangesMerged
  lifecycleStatsAbandonedHisto <- runMetricTrendIntPB metricChangesAbandoned

  merged <- wordToCount <$> runMetricNum metricChangesMerged
  selfMerged <- runMetricNum metricChangesSelfMerged
  abandoned <- wordToCount <$> runMetricNum metricChangesAbandoned
  created <- wordToCount <$> runMetricNum metricChangesCreated
  changeCreatedAuthor <- runMetricNum metricChangeAuthors

  lifecycleStatsTtmMean <- runMetricNum metricTimeToMerge
  lifecycleStatsTtmVariability <- runMetricNum metricTimeToMergeVariance
  lifecycleStatsUpdatesOfChanges <- runMetricNum metricChangeUpdates
  lifecycleStatsCommitsPerChange <- runMetricNum metricCommitsPerChange

  tests <- wordToCount <$> runMetricNum metricChangeWithTests

  let lifecycleStatsMerged = countToWord merged
      lifecycleStatsSelfMerged = selfMerged
      lifecycleStatsSelfMergedRatio = wordToCount selfMerged `ratioF` merged
      lifecycleStatsAbandoned = countToWord abandoned
      lifecycleStatsCreated = Just $ SearchPB.ReviewCount changeCreatedAuthor (countToWord created)
      lifecycleStatsChangesWithTests = tests `ratioF` created
      lifecycleStatsIterationsPerChange = wordToCount lifecycleStatsUpdatesOfChanges `ratioN` created

  pure $ SearchPB.LifecycleStats {..}
 where
  ratio :: Deci -> Count -> Count -> Deci
  ratio m x y
    | y == 0 = 0
    | otherwise = m * countToDeci x / countToDeci y
  ratioF x = fromFixed . ratio 100 x
  ratioN x = fromFixed . ratio 1 x

getActivityStats :: QueryMonad m => m SearchPB.ActivityStats
getActivityStats = do
  changeCreatedHisto <- runMetricTrendIntPB metricChangeAuthors
  changeCommentedHisto <- runMetricTrendIntPB metricCommentAuthors
  changeReviewedHisto <- runMetricTrendIntPB metricReviewAuthors

  changeAuthorsCount <- runMetricNum metricChangeAuthors
  commentAuthorsCount <- runMetricNum metricCommentAuthors
  reviewAuthorsCount <- runMetricNum metricReviewAuthors

  let activityStatsChangeAuthors = changeAuthorsCount
      activityStatsCommentAuthors = commentAuthorsCount
      activityStatsReviewAuthors = reviewAuthorsCount
      activityStatsCommentsHisto = changeCommentedHisto
      activityStatsReviewsHisto = changeReviewedHisto
      activityStatsChangesHisto = changeCreatedHisto
  pure $ SearchPB.ActivityStats {..}

getSuggestions :: QueryMonad m => Config.Index -> m SearchPB.SuggestionsResponse
getSuggestions index = do
  suggestionsResponseTaskTypes <- getTop "tasks_data.ttype"
  suggestionsResponseAuthors <- getTop "author.muid"
  suggestionsResponseApprovals <- getTop "approval"
  suggestionsResponsePriorities <- getTop "tasks_data.priority"
  suggestionsResponseSeverities <- getTop "tasks_data.severity"
  suggestionsResponseLabels <- getTop "labels"
  let suggestionsResponseProjects = V.fromList $ from <$> Config.getTenantProjectsNames index
      suggestionsResponseGroups = V.fromList $ from . fst <$> Config.getTenantGroups index

  pure $ SearchPB.SuggestionsResponse {..}
 where
  getTop field' = do
    tt <- getDocTypeTopCountByField (EChangeDoc :| []) field' (Just 1000)
    pure $ V.fromList $ from . trTerm <$> tsrTR tt

-------------------------------------------------------------------------------
-- The final metrics

newtype Numeric a = Num {unNum :: a}

instance Functor Numeric where
  fmap f (Num n) = Num $ f n

data Histo a = Histo
  { hDate :: String
  , hValue :: a
  }

instance Functor Histo where
  fmap f (Histo d n) = Histo d $ f n

data TermCount a = TermCount
  { tcTerm :: Text
  , tcCount :: a
  }

instance Functor TermCount where
  fmap f (TermCount {..}) = TermCount tcTerm (f tcCount)

data TermsCount a = TermsCount
  { tscData :: V.Vector (TermCount a)
  , tscTotalHits :: Word32
  }

instance Functor TermsCount where
  fmap f (TermsCount {..}) = TermsCount (fmap f <$> tscData) tscTotalHits

data MetricInfo = MetricInfo
  { miMetricName :: Text
  , miName :: Text
  , miDesc :: Text
  , miLongDesc :: Maybe Text
  }

data Metric m a = Metric
  { metricInfo :: MetricInfo
  , runMetric :: m (Numeric a)
  , runMetricTrend :: Maybe Q.TimeRange -> m (V.Vector (Histo a))
  , runMetricTop :: m (Maybe (TermsCount a))
  }

instance (Functor m) => Functor (Metric m) where
  fmap f Metric {..} =
    Metric
      metricInfo
      (fmap f <$> runMetric)
      (const $ (fmap . fmap) f <$> runMetricTrend Nothing)
      ((fmap . fmap) f <$> runMetricTop)

runMetricNum :: QueryMonad m => Metric m a -> m a
runMetricNum m = unNum <$> runMetric m

toPBHistoInt :: Histo Word32 -> MetricPB.HistoInt
toPBHistoInt (Histo {..}) =
  let histoIntDate = from hDate
      histoIntCount = hValue
   in MetricPB.HistoInt {..}

toPBHistoFloat :: Histo Float -> MetricPB.HistoFloat
toPBHistoFloat (Histo {..}) =
  let histoFloatDate = from hDate
      histoFloatCount = hValue
   in MetricPB.HistoFloat {..}

toPBTermsCountInt :: TermsCount Word32 -> MetricPB.TermsCountInt
toPBTermsCountInt (TermsCount {..}) =
  let termsCountIntTermcount = fmap toPBTermCountInt tscData
      termsCountIntTotalHits = tscTotalHits
   in MetricPB.TermsCountInt {..}
 where
  toPBTermCountInt TermCount {..} =
    let termCountIntCount = tcCount
        termCountIntTerm = from tcTerm
     in MetricPB.TermCountInt {..}

toPBTermsCountFloat :: TermsCount Float -> MetricPB.TermsCountFloat
toPBTermsCountFloat (TermsCount {..}) =
  let termsCountFloatTermcount = fmap toPBTermCountFloat tscData
      termsCountFloatTotalHits = tscTotalHits
   in MetricPB.TermsCountFloat {..}
 where
  toPBTermCountFloat TermCount {..} =
    let termCountFloatCount = tcCount
        termCountFloatTerm = from tcTerm
     in MetricPB.TermCountFloat {..}

runMetricTrendIntPB :: QueryMonad m => Metric m Word32 -> m (V.Vector MetricPB.HistoInt)
runMetricTrendIntPB m = fmap toPBHistoInt <$> runMetricTrend m Nothing

authorFlavorToDesc :: AuthorFlavor -> Text
authorFlavorToDesc = \case
  Author ->
    "The event's author is matched in case of any author/group query filter."
  OnAuthor ->
    "The change's author is matched in case of any author/group query filter."

rangeFlavorToDesc :: RangeFlavor -> Text
rangeFlavorToDesc = \case
  CreatedAt -> "The event's creation date is matched in case of any date query filter."
  OnCreatedAt -> "The change's creation date is matched in case of any date query filter."
  UpdatedAt -> "The change's update date is matched in case of any date query filter."
  OnCreatedAndCreated ->
    "Both, the event and change's creation date is matched in "
      <> "case of any date query filter."

queryToHistoBounds :: QueryMonad m => Maybe Q.TimeRange -> m (UTCTime, UTCTime, Q.TimeRange)
queryToHistoBounds intervalM = do
  query <- getQuery
  let (minDate, maxDate) = Q.queryBounds query
      autoInterval = from @Pico @Q.TimeRange $ elapsedSeconds minDate maxDate
      interval = fromMaybe autoInterval intervalM
  pure (minDate, maxDate, interval)

monoHisto :: forall m a. QueryMonad m => Maybe Q.TimeRange -> m a -> m (V.Vector (Histo a))
monoHisto intervalM metric = do
  (minDate, maxDate, interval) <- queryToHistoBounds intervalM
  let sliceBounds = mkSliceBounds maxDate interval [sliceBound minDate interval]
  traverse (runMetricOnSlice interval) $ fromList sliceBounds
 where
  sliceBound :: UTCTime -> Q.TimeRange -> (UTCTime, UTCTime)
  sliceBound fromDate@(UTCTime days _) interval =
    ( fromDate
    , case interval of
        Q.Hour -> addUTCTime anHour fromDate
        Q.Day -> UTCTime (addDays 1 days) (secondsToDiffTime 0)
        Q.Week -> UTCTime (addDays 7 days) (secondsToDiffTime 0)
        Q.Month -> UTCTime (addGregorianMonthsClip 1 days) (secondsToDiffTime 0)
        Q.Year -> UTCTime (addGregorianYearsClip 1 days) (secondsToDiffTime 0)
    )
   where
    anHour = secondsToNominalDiffTime 3600
  mkSliceBounds :: UTCTime -> Q.TimeRange -> [(UTCTime, UTCTime)] -> [(UTCTime, UTCTime)]
  mkSliceBounds maxDate interval acc =
    case acc of
      [] -> error "Impossible case"
      xs | snd (Data.List.last xs) >= maxDate -> acc
      xs ->
        let newMin = snd (Data.List.last xs)
            newAcc = acc <> [sliceBound newMin interval]
         in mkSliceBounds maxDate interval newAcc
  runMetricOnSlice :: Q.TimeRange -> (UTCTime, UTCTime) -> m (Histo a)
  runMetricOnSlice interval bounds =
    toHisto <$> withModified Q.dropDate (withFilter boundsBH metric)
   where
    toHisto :: a -> Histo a
    toHisto = Histo (from $ dateInterval interval $ fst bounds)
    boundsBH :: [BH.Query]
    boundsBH =
      let rq c = BH.QueryRangeQuery $ BH.mkRangeQuery (BH.FieldName "created_at") c
       in [ rq $ BH.RangeDateGte (BH.GreaterThanEqD (fst bounds))
          , rq $ BH.RangeDateLt (BH.LessThanD (snd bounds))
          ]

countHisto :: forall m. QueryMonad m => RangeFlavor -> Maybe Q.TimeRange -> m (V.Vector (Histo Word32))
countHisto rf intervalM = fmap toHisto <$> getCountHisto
 where
  toHisto :: HistoSimple -> Histo Word32
  toHisto HistoBucket {..} =
    let hDate = from hbDate
        hValue = hbCount
     in Histo {..}
  getCountHisto :: m (V.Vector HistoSimple)
  getCountHisto = do
    queryBH <- getQueryBH
    (minDate, maxDate, interval) <- queryToHistoBounds intervalM

    let bound =
          Aeson.object
            [ "min" .= dateInterval interval minDate
            , "max" .= dateInterval interval maxDate
            ]
        date_histo =
          Aeson.object
            [ "field" .= rangeField rf
            , "calendar_interval" .= into @Text interval
            , "format" .= getFormat (from interval)
            , "min_doc_count" .= (0 :: Word)
            , "extended_bounds" .= bound
            ]
        agg =
          Aeson.object
            [ "agg1" .= Aeson.object ["date_histogram" .= date_histo]
            ]
        search =
          Aeson.object
            [ "aggregations" .= agg
            , "size" .= (0 :: Word)
            , "query" .= fromMaybe (error "need query") queryBH
            ]

    hBuckets . parseAggregationResults "agg1" <$> doAggregation search

authorCountHisto :: forall m. QueryMonad m => EDocType -> Maybe Q.TimeRange -> m (V.Vector (Histo Word32))
authorCountHisto changeEvent intervalM = withDocType changeEvent qf getAuthorHistoPB
 where
  qf = QueryFlavor Author CreatedAt
  getAuthorHistoPB = fmap toHisto <$> getAuthorCountHisto
  toHisto HistoBucket {..} =
    let hDate = from hbDate
        hValue =
          fromInteger
            . toInteger
            . length
            . haBuckets
            . fromMaybe (error "subbucket not found")
            $ hbSubBuckets
     in Histo {..}
  getAuthorCountHisto :: m (V.Vector (HistoBucket HistoAuthors))
  getAuthorCountHisto = do
    queryBH <- getQueryBH
    (minDate, maxDate, interval) <- queryToHistoBounds intervalM

    let bound =
          Aeson.object
            [ "min" .= dateInterval interval minDate
            , "max" .= dateInterval interval maxDate
            ]
        date_histo =
          Aeson.object
            [ "field" .= rangeField (qfRange qf)
            , "calendar_interval" .= into @Text interval
            , "format" .= getFormat (from interval)
            , "min_doc_count" .= (0 :: Word)
            , "extended_bounds" .= bound
            ]
        author_agg =
          Aeson.object
            [ "authors"
                .= Aeson.object
                  [ "terms"
                      .= Aeson.object
                        [ "field" .= ("author.muid" :: Text)
                        , "size" .= (10000 :: Word)
                        ]
                  ]
            ]
        agg =
          Aeson.object
            [ "agg1"
                .= Aeson.object
                  [ "date_histogram" .= date_histo
                  , "aggs" .= author_agg
                  ]
            ]
        search =
          Aeson.object
            [ "aggregations" .= agg
            , "size" .= (0 :: Word)
            , "query" .= fromMaybe (error "need query") queryBH
            ]

    hBuckets . parseAggregationResults "agg1" <$> doAggregation search

changeEventCount :: QueryMonad m => MetricInfo -> EDocType -> Metric m Word32
changeEventCount mi dt =
  Metric mi (Num . countToWord <$> compute) computeTrend (pure Nothing)
 where
  compute = withFilter [documentType dt] (withFlavor qf countDocs)
  computeTrend interval = withDocType EChangeCreatedEvent qf $ countHisto CreatedAt interval
  qf = QueryFlavor OnAuthor CreatedAt

changeEventFlavorDesc :: Text
changeEventFlavorDesc = authorFlavorToDesc OnAuthor <> " " <> rangeFlavorToDesc CreatedAt

metricChangesCreated :: QueryMonad m => Metric m Word32
metricChangesCreated = changeEventCount mi EChangeCreatedEvent
 where
  mi =
    MetricInfo
      "changes_created"
      "Changes created count"
      "The count of changes created"
      ( Just $
          "The metric is the count change created events."
            <> changeEventFlavorDesc
      )

metricChangesMerged :: QueryMonad m => Metric m Word32
metricChangesMerged = changeEventCount mi EChangeMergedEvent
 where
  mi =
    MetricInfo
      "changes_merged"
      "Changes merged count"
      "The count of changes merged"
      ( Just $
          "The metric is the count change merged events."
            <> changeEventFlavorDesc
      )

metricChangesAbandoned :: QueryMonad m => Metric m Word32
metricChangesAbandoned = changeEventCount mi EChangeAbandonedEvent
 where
  mi =
    MetricInfo
      "changes_abandoned"
      "Changes abandoned count"
      "The count of changes abandoned"
      ( Just $
          "The metric is the count change abandoned events."
            <> changeEventFlavorDesc
      )

metricChangeUpdates :: QueryMonad m => Metric m Word32
metricChangeUpdates = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "change_updates"
      "Change updates count"
      "The count of updates of changes"
      ( Just $
          "The metric is the count of commit push and force commit push events. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc OnCreatedAt
      )
  compute = Num . countToWord <$> withFilter [documentTypes $ fromList docs] (withFlavor qf countDocs)
  computeTrend interval = withDocTypes docs qf $ countHisto CreatedAt interval
  qf = QueryFlavor Author OnCreatedAt
  docs = [EChangeCommitPushedEvent, EChangeCommitForcePushedEvent]

-- | The count of changes with tests related modifications
metricChangeWithTests :: QueryMonad m => Metric m Word32
metricChangeWithTests =
  Metric
    ( MetricInfo
        "change_with_tests_count"
        "Change with tests count"
        "The count of changes with tests modifications"
        ( Just $
            "The metric is the count of changes with modification on files named based on the following regex: "
              <> regexp
              <> " "
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    countToWord
      <$> withFilter [documentType EChangeDoc, testIncluded] (withFlavor' countDocs)
  withFlavor' = withFlavor (QueryFlavor Author CreatedAt)
  regexp = ".*[Tt]est.*"
  testIncluded =
    BH.QueryRegexpQuery $
      BH.RegexpQuery (BH.FieldName "changed_files.path") (BH.Regexp regexp) BH.AllRegexpFlags Nothing
  computeTrend = flip monoHisto compute

-- | The count of changes self merged
metricChangesSelfMerged :: QueryMonad m => Metric m Word32
metricChangesSelfMerged =
  Metric
    ( MetricInfo
        "changes_self_merged_count"
        "Changes self merged count"
        "The count of changes self merged"
        ( Just $
            "The metric is the count change merged events for which "
              <> "the event's author and the change's author are the same. "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute = countToWord <$> withFilter selfMerged (withFlavor' countDocs)
  selfMerged =
    [ documentType EChangeMergedEvent
    , BH.TermQuery (BH.Term "self_merged" "true") Nothing
    ]
  withFlavor' = withFlavor (QueryFlavor Author CreatedAt)
  computeTrend = flip monoHisto compute

metricReviews :: QueryMonad m => Metric m Word32
metricReviews = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "reviews"
      "Reviews count"
      "The count of change' reviews"
      ( Just $
          "The metric is the count change' code reviews. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter
        [documentType EChangeReviewedEvent]
        ( withFlavor qf countDocs
        )
  computeTrend interval = withDocType EChangeReviewedEvent qf $ countHisto CreatedAt interval
  qf = QueryFlavor Author CreatedAt

metricReviewsAndComments :: QueryMonad m => Metric m Word32
metricReviewsAndComments = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "reviews_and_comments"
      "Reviews and comments count"
      "The count of change' reviews + comments"
      ( Just $
          "The metric is the count change' code reviews + comments. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter
        [documentTypes $ fromList docs]
        (withFlavor qf countDocs)
  computeTrend interval = withDocTypes docs qf $ countHisto CreatedAt interval
  qf = QueryFlavor Author CreatedAt
  docs = [EChangeReviewedEvent, EChangeCommentedEvent]

metricComments :: QueryMonad m => Metric m Word32
metricComments = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "comments"
      "Comments count"
      "The count of change' comments"
      ( Just $
          "The metric is the count of change' comments. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter [documentType EChangeCommentedEvent] (withFlavor qf countDocs)
  computeTrend interval = withDocType EChangeCommentedEvent qf $ countHisto CreatedAt interval
  qf = QueryFlavor Author CreatedAt

metricReviewAuthors :: QueryMonad m => Metric m Word32
metricReviewAuthors = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "review_authors"
      "Review authors count"
      "The count of change's review authors"
      ( Just $
          "The metric is the count of change' reviews aggregated by unique authors. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter [documentType EChangeReviewedEvent] (withFlavor qf countAuthors)
  computeTrend = authorCountHisto EChangeReviewedEvent
  qf = QueryFlavor Author CreatedAt

metricCommentAuthors :: QueryMonad m => Metric m Word32
metricCommentAuthors = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "comment_authors"
      "Comment authors count"
      "The count of change's comment authors"
      ( Just $
          "The metric is the count of change' comments aggregated by unique authors. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter [documentType EChangeCommentedEvent] (withFlavor qf countAuthors)
  computeTrend = authorCountHisto EChangeCommentedEvent
  qf = QueryFlavor Author CreatedAt

metricChangeAuthors :: QueryMonad m => Metric m Word32
metricChangeAuthors = Metric mi compute computeTrend (pure Nothing)
 where
  mi =
    MetricInfo
      "change_authors"
      "Change authors count"
      "The count of change's authors"
      ( Just $
          "The metric is the count of change created events aggregated by unique authors. "
            <> authorFlavorToDesc Author
            <> " "
            <> rangeFlavorToDesc CreatedAt
      )
  compute =
    Num . countToWord
      <$> withFilter [documentType EChangeCreatedEvent] (withFlavor qf countAuthors)
  computeTrend = authorCountHisto EChangeCreatedEvent
  qf = QueryFlavor Author CreatedAt

-- | The average duration for an open change to be merged
metricTimeToMerge :: QueryMonad m => Metric m Float
metricTimeToMerge =
  Metric
    ( MetricInfo
        "time_to_merge"
        "Time to merge"
        "The average duration for an open change to be merged"
        ( Just $
            "The metric is the average duration for changes "
              <> "between their creation date and their merge date."
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    double2Float
      <$> withFilter (changeState EChangeMerged) (averageDuration $ QueryFlavor Author CreatedAt)
  computeTrend = flip monoHisto compute

-- | The variance of the duration for an open change to be merged
metricTimeToMergeVariance :: QueryMonad m => Metric m Float
metricTimeToMergeVariance =
  Metric
    ( MetricInfo
        "time_to_merge_variance"
        "Time to merge variance"
        "The variance of the duration for an open change to be merged"
        ( Just $
            "The metric is the variance of the duration for changes "
              <> "between their creation date and their merge date."
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    double2Float
      <$> withFilter (changeState EChangeMerged) (medianDeviationDuration $ QueryFlavor Author CreatedAt)
  computeTrend = flip monoHisto compute

-- | The average duration until a change gets a first review event
metricFirstReviewMeanTime :: QueryMonad m => Metric m Word32
metricFirstReviewMeanTime =
  Metric
    ( MetricInfo
        "first_review_mean_time"
        "1st review mean time"
        "The average duration until a change gets a first review event"
        ( Just $
            "The metric is the average duration for changes to get their first review."
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    firstEventAverageDuration
      <$> withEvents [documentType EChangeReviewedEvent] (withFlavor' firstEventOnChanges)
  withFlavor' = withFlavor (QueryFlavor Author CreatedAt)
  computeTrend = flip monoHisto compute

-- | The average delay until a change gets a comment event
metricFirstCommentMeanTime :: QueryMonad m => Metric m Word32
metricFirstCommentMeanTime =
  Metric
    ( MetricInfo
        "first_comment_mean_time"
        "1st comment mean time"
        "The average delay until a change gets a comment event"
        ( Just $
            "The metric is the average duration for changes to get their first comment."
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    firstEventAverageDuration
      <$> withEvents [documentType EChangeCommentedEvent] (withFlavor' firstEventOnChanges)
  withFlavor' = withFlavor (QueryFlavor Author CreatedAt)
  computeTrend = flip monoHisto compute

-- | The average commit count for per change
metricCommitsPerChange :: QueryMonad m => Metric m Float
metricCommitsPerChange =
  Metric
    ( MetricInfo
        "commits_per_change"
        "commits per change"
        "The average commits count per merged change"
        ( Just $
            "The metric is the average of the number of commits a merged change is composed of."
              <> authorFlavorToDesc Author
              <> " "
              <> rangeFlavorToDesc CreatedAt
        )
    )
    (Num <$> compute)
    computeTrend
    (pure Nothing)
 where
  compute =
    double2Float
      <$> withFilter (changeState EChangeMerged) (changeMergedAvgCommits qf)
  qf = QueryFlavor Author CreatedAt
  computeTrend = flip monoHisto compute

allMetrics :: [MetricInfo]
allMetrics =
  map
    metricInfo
    [ toJSON <$> metricChangesCreated @QueryM
    , toJSON <$> metricChangesMerged
    , toJSON <$> metricChangesAbandoned
    , toJSON <$> metricChangesSelfMerged
    , toJSON <$> metricChangeUpdates
    , toJSON <$> metricChangeWithTests
    , toJSON <$> metricReviews
    , toJSON <$> metricComments
    , toJSON <$> metricReviewsAndComments
    , toJSON <$> metricReviewAuthors
    , toJSON <$> metricCommentAuthors
    , toJSON <$> metricChangeAuthors
    , toJSON <$> metricTimeToMerge
    , toJSON <$> metricTimeToMergeVariance
    , toJSON <$> metricFirstCommentMeanTime
    , toJSON <$> metricFirstReviewMeanTime
    , toJSON <$> metricCommitsPerChange
    ]
