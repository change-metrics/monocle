{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for low-level function unavailable in bloodhound
module Database.Bloodhound.Raw (
  ScrollRequest (..),
  TermsCompositeAggResult (..),
  TermsCompositeAggKey (..),
  TermsCompositeAggBucket (..),
  advance,
  search,
  searchHit,
  settings,
  aggWithDocValues,
  mkAgg,
  mkTermsCompositeAgg,
)
where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing.Internal qualified as AesonCasing
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Database.Bloodhound.Common.Requests qualified as Query
import Json.Extras qualified as Json
import Monocle.Prelude
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Method qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP

data ScrollRequest = NoScroll | GetScroll ByteString

-- | Utility function to advance in scroll result. We can use the BH library
--   because we no longer need to support a custom raw body once we have a scroll.
advance :: (MonadBH m, FromJSON resp) => BH.ScrollId -> m (BH.SearchResult resp)
advance scroll = do
  resp <- BH.tryEsError $ BH.advanceScroll scroll 60
  case resp of
    Left err -> throwEsError "advance" err
    Right x -> pure x

throwEsError :: MonadThrow m => LByteString -> BH.EsError -> m a
throwEsError resp err = throwM $ BH.EsProtocolException err.errorMessage resp

settings :: (MonadBH m, ToJSON body) => BH.IndexName -> body -> m ()
settings index body = do
  let endpoint = BH.mkEndpoint [BH.unIndexName index, "_settings"]
      method = HTTP.methodPut
  resp <- BH.performBHRequest @_ @BH.StatusIndependant $ BH.mkFullRequest method endpoint (Aeson.encode body)
  case resp of
    BH.Acknowledged True -> pure ()
    _ -> error $ "Settings apply failed: " <> show resp

-- | Manual aeson casing implementation to create the search _source attribute
--
-- >>> aesonCasing "echangeCommitCount"
-- "commit_count"
aesonCasing :: String -> String
aesonCasing = AesonCasing.snakeCase . AesonCasing.dropFPrefix

search ::
  forall resp m.
  MonadBH m =>
  FromJSONField resp =>
  BH.IndexName ->
  BH.Search ->
  ScrollRequest ->
  m (BH.SearchResult resp)
search index payload scrollRequest = do
  let query = (Query.searchByIndex index payload {BH.source = Just fields}) {BH.bhRequestQueryStrings = qs}
  resp <- BH.tryEsError $ BH.performBHRequest query
  case resp of
    Left err -> throwEsError "search" err
    Right x -> pure x
 where
  toSourceElem :: String -> String
  toSourceElem = from . aesonCasing

  -- The fields of the result data types.
  fields :: BH.Source
  fields = BH.SourcePatterns $ BH.PopPatterns $ BH.Pattern . T.pack . toSourceElem <$> selectors (Proxy :: Proxy (Rep resp))

  qs = case scrollRequest of
    NoScroll -> []
    GetScroll x -> [("scroll", Just x)]

-- | A special purpose search implementation that uses the faster json-syntax
searchHit ::
  MonadBH m =>
  (Json.Value -> Either Text a) ->
  BH.IndexName ->
  BH.Search ->
  m [a]
searchHit parseHit index payload = do
  let query = Query.searchByIndex @Value index payload
  resp <-
    BH.tryEsError $
    BH.performBHRequest @_ @BH.StatusIndependant query {
      BH.bhRequestParser = \(BH.BHResponse rawResp) ->
        let
          decodeHits :: Json.Value -> Maybe [Json.Value]
          decodeHits value = do
            hits <- Json.getAttr "hits" =<< Json.getAttr "hits" value
            fmap getSource <$> Json.getArray hits
          getSource value = case Json.getAttr "_source" value of
            Nothing -> error $ "No source found in: " <> show value
            Just v -> v
        in case decodeHits (Json.decodeThrow $ HTTP.responseBody rawResp) of
            Just xs -> pure $ first (BH.EsError $ Just $ HTTP.statusCode $ HTTP.responseStatus rawResp) $ traverse parseHit xs
            Nothing -> error $ "Could not find hits in " <> show rawResp
    }
  case resp of
    Right xs -> pure xs
    Left e -> throwEsError "Could not find hits" e

aggWithDocValues :: BH.Aggregations -> Maybe BH.Query -> BH.Search
aggWithDocValues agg = mkAgg agg (Just [BH.DocvalueFieldNameAndFormat (BH.FieldName "created_at") "date_time"])

mkAgg :: BH.Aggregations -> Maybe [BH.DocvalueField] -> Maybe BH.Query -> BH.Search
mkAgg agg docvalues query =
  BH.Search
    { trackSortScores = False
    , suggestBody = Nothing
    , sortBody = Nothing
    , searchType = BH.SearchTypeQueryThenFetch
    , searchAfterKey = Nothing
    , scriptFields = Nothing
    , docvalueFields = docvalues
    , queryBody = query
    , pointInTime = Nothing
    , highlight = Nothing
    , filterBody = Nothing
    , aggBody = Just agg
    , source = Nothing
    , size = BH.Size 0
    , fields = Nothing
    , from = BH.From 0
    }

mkTermsCompositeAgg :: Text -> Maybe Value -> BH.Aggregations
mkTermsCompositeAgg term afterM =
  Map.singleton
    "agg1"
    $ BH.CompositeAgg
    $ BH.CompositeAggregation
      { compositeAggregationSize = Just 1024
      , compositeAggregationSources =
          [ BH.CompositeAggregationSource
              "agg"
              $ BH.CompositeTermsAgg
                BH.TermsAggregation
                  { term = Right term
                  , termInclude = Nothing
                  , termExclude = Nothing
                  , termOrder = Nothing
                  , termMinDocCount = Nothing
                  , termSize = Nothing
                  , termShardSize = Nothing
                  , termCollectMode = Nothing
                  , termExecutionHint = Nothing
                  , termAggs = Nothing
                  }
          ]
      , compositeAggregationAfter = (\v -> Aeson.object ["agg" .= v]) <$> afterM
      }

-- Make Value a type parameter for TermsCompositeAggResult
newtype TermsCompositeAggKey = TermsCompositeAggKey
  {tcaaKey :: Value}
  deriving (Eq, Show)

instance FromJSON TermsCompositeAggKey where
  parseJSON (Object v) =
    TermsCompositeAggKey <$> v .: "agg"
  parseJSON _ = mzero

data TermsCompositeAggBucket = TermsCompositeAggBucket
  { tcaKey :: TermsCompositeAggKey
  , tcaDocCount :: Word32
  }
  deriving (Eq, Show)

instance FromJSON TermsCompositeAggBucket where
  parseJSON (Object v) =
    TermsCompositeAggBucket <$> v .: "key" <*> v .: "doc_count"
  parseJSON _ = mzero

data TermsCompositeAggResult = TermsCompositeAggResult
  { tcarAfterKey :: Maybe TermsCompositeAggKey
  , tcarBuckets :: V.Vector TermsCompositeAggBucket
  }
  deriving (Eq, Show)

instance FromJSON TermsCompositeAggResult where
  parseJSON (Object v) =
    TermsCompositeAggResult <$> v .:? "after_key" <*> v .: "buckets"
  parseJSON _ = mzero
