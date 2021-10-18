-- | Monocle queries
-- The goal of this module is to transform 'Query' into list of items
module Monocle.Backend.Queries where

import Data.Aeson (Value (Object), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Database.Bloodhound.Raw as BHR
import qualified Json.Extras as Json
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents (EChange (..), EChangeEvent (..), EChangeState (..), EDocType (..), allEventTypes)
import Monocle.Env
import Monocle.Prelude hiding (doSearch)
import qualified Monocle.Search as SearchPB
import Monocle.Search.Query (AuthorFlavor (..), QueryFlavor (..), RangeFlavor (..), rangeField)
import qualified Monocle.Search.Query as Q
import qualified Streaming.Prelude as Streaming

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
doScrollSearchBH :: (QueryMonad m, ToJSON body, FromJSON resp) => BHR.ScrollRequest -> body -> m (BH.SearchResult resp)
doScrollSearchBH scrollRequest body = do
  measureQueryM body $ do
    index <- getIndexName
    elasticSearch index body scrollRequest

-- | A search without scroll
doSearchBH :: (QueryMonad m, ToJSON body, FromJSON resp) => body -> m (BH.SearchResult resp)
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

-------------------------------------------------------------------------------
-- Mid level queries

-- | scan search the result using a streaming
scanSearch :: FromJSON resp => Stream (Of (BH.Hit resp)) QueryM ()
scanSearch = do
  resp <- lift $ do
    query <- getQueryBH
    doScrollSearchBH (BHR.GetScroll "1m") (BH.mkSearch query Nothing)
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
scanSearchHit :: FromJSON resp => Stream (Of resp) QueryM ()
scanSearchHit = Streaming.concat $ Streaming.map BH.hitSource $ scanSearch

-- | scan search the document id, here is an example usage for the REPL:
-- λ> testQueryM (defaultTenant "zuul") $ runQueryM (mkQuery []) $ Streaming.print scanSearchId
-- DocId ...
-- DocId ...
scanSearchId :: Stream (Of BH.DocId) QueryM ()
scanSearchId = Streaming.map BH.hitDocId $ anyScan
  where
    -- here we need to help ghc figures out what fromJSON to use
    anyScan :: Stream (Of (BH.Hit (Value))) QueryM ()
    anyScan = scanSearch

scanSearchSimple :: FromJSON resp => QueryM [resp]
scanSearchSimple = Streaming.toList_ scanSearchHit

-- | Get search results hits
doSearch :: QueryMonad m => FromJSON resp => Maybe SearchPB.Order -> Word32 -> m [resp]
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
  value <- Map.lookup key res `orDie` ("No value found for: " <> toString key)
  Aeson.parseEither Aeson.parseJSON value

queryAggResult :: QueryMonad m => FromJSON a => Value -> m a
queryAggResult body = parseAggregationResults "agg1" <$> doAggregation body

-------------------------------------------------------------------------------
-- High level queries
changes :: QueryMonad m => Maybe SearchPB.Order -> Word32 -> m [EChange]
changes orderM limit =
  withDocTypes [EChangeDoc] (QueryFlavor Author UpdatedAt) $
    doSearch orderM limit

changeEvents :: QueryMonad m => LText -> Word32 -> m (EChange, [EChangeEvent])
changeEvents changeID limit = dropQuery $
  withFilter [mkTerm "change_id" (toText changeID)] $ do
    change <- fromMaybe (error "Unknown change") . headMaybe <$> changes Nothing 1

    -- Collect all the events
    result <- withDocTypes allEventTypes (QueryFlavor Author CreatedAt) $ do
      doSearch Nothing limit

    pure (change, result)

-- | The change created / review ratio
changeReviewRatio :: QueryMonad m => m Float
changeReviewRatio = withFlavor qf $ do
  commitCount <- withFilter [documentType EChangeCreatedEvent] $ countDocs
  reviewCount <-
    withFilter [documentTypes $ fromList [EChangeReviewedEvent, EChangeCommentedEvent]] $
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
changeState :: EChangeState -> [BH.Query]
changeState state' =
  [ BH.TermQuery (BH.Term "type" "Change") Nothing,
    BH.TermQuery (BH.Term "state" $ from state') Nothing
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
documentTypes :: NonEmpty EDocType -> BH.Query
documentTypes doc = BH.TermsQuery "type" $ from <$> doc

documentType :: EDocType -> BH.Query
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

getEventCounts :: QueryMonad m => m EventCounts
getEventCounts =
  -- TODO: ensure the right flavor is used
  EventCounts
    <$> withFilter (changeState EChangeOpen) countDocs
      <*> withFilter (changeState EChangeMerged) countDocs
      <*> withFilter (changeState EChangeClosed) countDocs
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
    hbSubBuckets :: Maybe a
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
        | subKeyName == "unused" = pure Nothing
        | otherwise = v .:? subKeyName
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

firstEventOnChanges :: QueryMonad m => m [FirstEvent]
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

getProjectAgg :: QueryMonad m => BH.Query -> m [EventProjectBucketAgg]
getProjectAgg query = do
  -- TODO: check why this is not calling the low-level function defined in this module
  res <- toAggRes <$> doSearchBH (BHR.aggWithDocValues agg (Just query))
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

mergedChangesCount :: QueryMonad m => m Count
mergedChangesCount =
  withFilter
    [documentType EChangeMergedEvent]
    (withFlavor (QueryFlavor OnAuthor CreatedAt) countDocs)

abandonedChangesCount :: QueryMonad m => m Count
abandonedChangesCount =
  withFilter
    [documentType EChangeAbandonedEvent]
    (withFlavor (QueryFlavor OnAuthor CreatedAt) countDocs)

selfMergedChangeCount :: QueryMonad m => m Count
selfMergedChangeCount = withFilter selfMerged countDocs

-- | The repos_summary query
getRepos :: QueryMonad m => m TermsResultWTH
getRepos =
  withFlavor (QueryFlavor Author CreatedAt) $
    getDocTypeTopCountByField (EChangeDoc :| []) "repository_fullname" (Just 5000)

data RepoSummary = RepoSummary
  { fullname :: Text,
    createdChanges :: Count,
    abandonedChanges :: Count,
    mergedChanges :: Count,
    updatedChanges :: Count,
    openChanges :: Count
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
      let eventQF = withFlavor (QueryFlavor OnAuthor CreatedAt)
          changeQF = withFlavor (QueryFlavor Author UpdatedAt)

      -- Count the events
      createdChanges <- withFilter [documentType EChangeCreatedEvent] (eventQF countDocs)
      updatedChanges <- withFilter (changeState EChangeOpen) (changeQF countDocs)
      mergedChanges <- mergedChangesCount
      openChanges <- openChangesCount
      abandonedChanges <- abandonedChangesCount

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
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (EChangeReviewedEvent :| []) "on_author.muid" (Just limit)

getMostCommentedAuthor :: QueryMonad m => Word32 -> m TermsResultWTH
getMostCommentedAuthor limit =
  withFlavor (QueryFlavor OnAuthor CreatedAt) $
    getDocTypeTopCountByField (EChangeCommentedEvent :| []) "on_author.muid" (Just limit)

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

data HistoInterval = Hour | Day | Week | Month | Year deriving (Eq, Show)

-- | Convert a duration to an interval that spans over maximum 24 buckets (31 for days)
instance From Pico HistoInterval where
  from sec
    | sec / day <= 1 = Hour
    | sec / month <= 1 = Day
    | sec / month <= 6 = Week
    | sec / year <= 2 = Month
    | otherwise = Year
    where
      year = month * 12
      month = day * 31
      day = 24 * 3600

newtype TimeFormat = TimeFormat {getFormat :: Text}

instance From HistoInterval TimeFormat where
  from hi = TimeFormat $ case hi of
    Hour -> "yyyy-MM-dd HH:mm"
    Day -> "yyyy-MM-dd"
    Week -> "yyyy-MM-dd"
    Month -> "yyyy-MM"
    Year -> "yyyy"

instance From HistoInterval Text where
  from = \case
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
getHisto :: QueryMonad m => RangeFlavor -> m (V.Vector HistoSimple)
getHisto rf = do
  query <- getQuery
  queryBH <- getQueryBH

  let (minDate, maxDate) = Q.queryBounds query
      duration = elapsedSeconds minDate maxDate
      interval = from duration

      bound =
        Aeson.object
          [ "min" .= dateInterval interval minDate,
            "max" .= dateInterval interval maxDate
          ]
      date_histo =
        Aeson.object
          [ "field" .= rangeField rf,
            "calendar_interval" .= into @Text interval,
            "format" .= getFormat (from interval),
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

getHistoPB :: QueryMonad m => RangeFlavor -> m (V.Vector SearchPB.Histo)
getHistoPB rf = fmap toPBHisto <$> getHisto rf
  where
    toPBHisto :: HistoSimple -> SearchPB.Histo
    toPBHisto HistoBucket {..} =
      let histoDate = hbDate
          histoCount = hbCount
       in SearchPB.Histo {..}

searchBody :: QueryMonad m => QueryFlavor -> Value -> m Value
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

-- | changes review stats
getReviewStats :: QueryMonad m => m SearchPB.ReviewStats
getReviewStats = do
  reviewStatsCommentHisto <- getHisto' EChangeCommentedEvent
  reviewStatsReviewHisto <- getHisto' EChangeReviewedEvent

  commentCount <- withFilter [documentType EChangeCommentedEvent] statCount
  reviewCount <- withFilter [documentType EChangeReviewedEvent] statCount

  let reviewStatsCommentCount = Just commentCount
      reviewStatsReviewCount = Just reviewCount
      withEvents ev = withFlavor (QueryFlavor Author OnCreatedAndCreated) . withFilter ev

  firstComments <- withEvents [documentType EChangeCommentedEvent] firstEventOnChanges
  firstReviews <- withEvents [documentType EChangeReviewedEvent] firstEventOnChanges

  let reviewStatsCommentDelay = firstEventAverageDuration firstComments
      reviewStatsReviewDelay = firstEventAverageDuration firstReviews

  pure $ SearchPB.ReviewStats {..}
  where
    qf = QueryFlavor Monocle.Search.Query.Author CreatedAt
    statCount :: QueryMonad m => m SearchPB.ReviewCount
    statCount =
      SearchPB.ReviewCount
        <$> fmap countToWord (withFlavor qf countAuthors)
        <*> fmap countToWord (withFlavor qf countDocs)

    getHisto' docType = withDocType docType qf (getHistoPB CreatedAt)

-- | changes lifecycle stats
getLifecycleStats :: QueryMonad m => m SearchPB.LifecycleStats
getLifecycleStats = do
  lifecycleStatsCreatedHisto <- getHisto' EChangeCreatedEvent
  lifecycleStatsUpdatedHisto <- getHistos' [EChangeCommitPushedEvent, EChangeCommitForcePushedEvent]
  lifecycleStatsMergedHisto <- getHisto' EChangeMergedEvent
  lifecycleStatsAbandonedHisto <- getHisto' EChangeAbandonedEvent

  (created, lifecycleStatsCreated) <- withDocType EChangeCreatedEvent qf $ do
    created <- countDocs
    stats <-
      SearchPB.ReviewCount
        <$> fmap countToWord countAuthors
        <*> pure (countToWord created)
    pure (created, Just stats)

  merged <- mergedChangesCount
  selfMerged' <- selfMergedChangeCount
  abandoned <- abandonedChangesCount

  let lifecycleStatsMerged = countToWord merged
      lifecycleStatsSelfMerged = countToWord selfMerged'
      lifecycleStatsSelfMergedRatio = selfMerged' `ratioF` merged
      lifecycleStatsAbandoned = countToWord abandoned

  lifecycleStatsTtmMean <-
    double2Float
      <$> withFilter (changeState EChangeMerged) (averageDuration qf)
  lifecycleStatsTtmVariability <-
    double2Float
      <$> withFilter (changeState EChangeMerged) (medianDeviationDuration qf)

  updated <-
    withFilter
      [documentTypes $ fromList [EChangeCommitPushedEvent, EChangeCommitForcePushedEvent]]
      countEvents

  let lifecycleStatsUpdatesOfChanges = countToWord updated

  tests <- withFilter [documentType EChangeDoc, testIncluded] countDocs
  let lifecycleStatsChangesWithTests = tests `ratioF` created
      lifecycleStatsIterationsPerChange = updated `ratioN` created

  lifecycleStatsCommitsPerChange <-
    double2Float
      <$> withFilter (changeState EChangeMerged) (changeMergedAvgCommits qf)

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
getAuthorHisto :: QueryMonad m => QueryFlavor -> m (V.Vector (HistoBucket HistoAuthors))
getAuthorHisto qf = withFlavor qf $ do
  query <- getQuery
  queryBH <- getQueryBH

  let (minDate, maxDate) = Q.queryBounds query
      duration = elapsedSeconds minDate maxDate
      interval = from duration

      bound =
        Aeson.object
          [ "min" .= dateInterval interval minDate,
            "max" .= dateInterval interval maxDate
          ]
      date_histo =
        Aeson.object
          [ "field" .= rangeField (qfRange qf),
            "calendar_interval" .= into @Text interval,
            "format" .= getFormat (from interval),
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

getActivityStats :: QueryMonad m => m SearchPB.ActivityStats
getActivityStats = do
  changeCreatedHisto <- getHisto' EChangeCreatedEvent
  changeCommentedHisto <- getHisto' EChangeCommentedEvent
  changeReviewedHisto <- getHisto' EChangeReviewedEvent

  changeAuthorsCount <- runCount EChangeCreatedEvent
  commentAuthorsCount <- runCount EChangeCommentedEvent
  reviewAuthorsCount <- runCount EChangeReviewedEvent

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
    getHistoPB' :: QueryMonad m => m (V.Vector SearchPB.Histo)
    getHistoPB' = fmap toPBHisto <$> getAuthorHisto qf
    toPBHisto :: HistoBucket HistoAuthors -> SearchPB.Histo
    toPBHisto HistoBucket {..} =
      let histoDate = hbDate
          histoCount =
            fromInteger
              . toInteger
              . length
              . haBuckets
              . fromMaybe (error "subbucket not found")
              $ hbSubBuckets
       in SearchPB.Histo {..}

getSuggestions :: QueryMonad m => Config.Index -> m SearchPB.SuggestionsResponse
getSuggestions index = do
  suggestionsResponseTaskTypes <- getTop "tasks_data.ttype"
  suggestionsResponseAuthors <- getTop "author.muid"
  suggestionsResponseApprovals <- getTop "approval"
  suggestionsResponsePriorities <- getTop "tasks_data.priority"
  suggestionsResponseSeverities <- getTop "tasks_data.severity"
  suggestionsResponseLabels <- getTop "labels"
  let suggestionsResponseProjects = V.fromList $ toLazy <$> Config.getTenantProjectsNames index
      suggestionsResponseGroups = V.fromList $ toLazy . fst <$> Config.getTenantGroups index

  pure $ SearchPB.SuggestionsResponse {..}
  where
    getTop field' = do
      tt <- getDocTypeTopCountByField (EChangeDoc :| []) field' (Just 1000)
      pure $ V.fromList $ toLazy . trTerm <$> tsrTR tt
