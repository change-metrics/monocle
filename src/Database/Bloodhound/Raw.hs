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
) where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing.Internal qualified as AesonCasing
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Json.Extras qualified as Json
import Monocle.Prelude
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Method qualified as HTTP

data ScrollRequest = NoScroll | GetScroll ByteString

type QS = [(ByteString, Maybe ByteString)]

dispatch ::
  BH.MonadBH m =>
  HTTP.Method ->
  Text ->
  LByteString ->
  QS ->
  m BH.Reply
dispatch method url body qs = do
  initReq <- liftIO $ HTTP.parseRequest (from url)
  let request =
        initReq
          { HTTP.method = method
          , HTTP.requestHeaders =
              ("Content-Type", "application/json") : HTTP.requestHeaders initReq
          , HTTP.requestBody = HTTP.RequestBodyLBS body
          }
  manager <- BH.bhManager <$> BH.getBHEnv
  liftIO $ HTTP.httpLbs (setQs request) manager
 where
  setQs = case qs of
    [] -> id
    xs -> HTTP.setQueryString xs

-- | Utility function to advance in scroll result. We can use the BH library
--   because we no longer need to support a custom raw body once we have a scroll.
advance :: (MonadBH m, MonadThrow m, FromJSON resp) => BH.ScrollId -> m (BH.SearchResult resp)
advance scroll = do
  resp <- BH.advanceScroll scroll 60
  case resp of
    Left e -> handleError e
    Right x -> pure x
 where
  handleError resp = do
    logText (show resp)
    error "Elastic scroll response failed"

settings :: (MonadBH m, ToJSON body) => BH.IndexName -> body -> m ()
settings (BH.IndexName index) body = do
  BH.Server s <- BH.bhServer <$> BH.getBHEnv
  let url = Text.intercalate "/" [s, index, "_settings"]
      method = HTTP.methodPut
  resp <- dispatch method url (Aeson.encode body) []
  case HTTP.responseBody resp of
    "{\"acknowledged\":true}" -> pure ()
    _ -> error $ "Settings apply failed: " <> show resp

search' :: (MonadBH m, ToJSON body) => BH.IndexName -> body -> QS -> m BH.Reply
search' (BH.IndexName index) body qs = do
  BH.Server s <- BH.bhServer <$> BH.getBHEnv
  let url = Text.intercalate "/" [s, index, "_search"]
      method = HTTP.methodPost
  dispatch method url (Aeson.encode body) qs

-- | Manual aeson casing implementation to create the search _source attribute
--
-- >>> aesonCasing "echangeCommitCount"
-- "commit_count"
aesonCasing :: String -> String
aesonCasing = AesonCasing.snakeCase . AesonCasing.dropFPrefix

search ::
  forall resp m body.
  (MonadBH m, MonadThrow m) =>
  (Aeson.ToJSON body, FromJSONField resp) =>
  BH.IndexName ->
  body ->
  ScrollRequest ->
  m (BH.SearchResult resp)
search index body scrollRequest = do
  rawResp <- search' index newBody qs
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left _e -> handleError rawResp
    Right x -> pure x
 where
  newBody = case (fields, toJSON body) of
    -- The results has fields, and the body is an object
    (xs@(_ : _), Aeson.Object obj) -> Aeson.Object $ addSourceFields xs obj
    -- Otherwise we don't change the body
    (_, bodyValue) -> bodyValue

  addSourceFields xs = KM.insert "_source" (Aeson.Array $ fromList $ map toSourceElem xs)

  toSourceElem :: String -> Value
  toSourceElem = Aeson.String . from . aesonCasing

  -- The fields of the result data types.
  fields :: [String]
  fields = selectors (Proxy :: Proxy (Rep resp))

  qs = case scrollRequest of
    NoScroll -> []
    GetScroll x -> [("scroll", Just x)]
  handleError resp = do
    logText (show resp)
    error "Elastic response failed"

-- | A special purpose search implementation that uses the faster json-syntax
searchHit ::
  (MonadBH m) =>
  (Aeson.ToJSON body) =>
  BH.IndexName ->
  body ->
  m [Json.Value]
searchHit index body = do
  rawResp <- search' index body []
  case decodeHits (Json.decodeThrow $ HTTP.responseBody rawResp) of
    Just xs -> pure xs
    Nothing -> error $ "Could not find hits in " <> show rawResp
 where
  decodeHits :: Json.Value -> Maybe [Json.Value]
  decodeHits value = do
    hits <- Json.getAttr "hits" =<< Json.getAttr "hits" value
    fmap getSource <$> Json.getArray hits
  getSource value = case Json.getAttr "_source" value of
    Nothing -> error $ "No source found in: " <> show value
    Just v -> v

aggWithDocValues :: [(Text, Value)] -> Maybe BH.Query -> Value
aggWithDocValues agg = mkAgg agg (Just dv)
 where
  dv =
    Aeson.Array
      ( V.fromList
          [ Aeson.object
              [ "field" .= Aeson.String "created_at"
              , "format" .= Aeson.String "date_time"
              ]
          ]
      )

toPair :: (Text, Value) -> Aeson.Pair
toPair (k, v) = (from k, v)

mkAgg :: [(Text, Value)] -> Maybe Value -> Maybe BH.Query -> Value
mkAgg agg docvalues query =
  Aeson.object $
    [ "aggregations" .= Aeson.object (toPair <$> agg)
    , "size" .= Aeson.Number 0
    ]
      <> case docvalues of
        Just dv -> ["docvalue_fields" .= dv]
        Nothing -> []
      <> case query of
        Just q -> ["query" .= Aeson.toJSON q]
        Nothing -> []

mkTermsCompositeAgg :: Text -> Maybe Value -> (Text, Value)
mkTermsCompositeAgg term afterM =
  ( "agg1"
  , Aeson.object
      [ "composite"
          .= Aeson.object
            ( [ "sources" .= [agg]
              , "size" .= Aeson.Number 1024
              ]
                <> after
            )
      ]
  )
 where
  after = case afterM of
    Just v -> ["after" .= Aeson.object ["agg" .= v]]
    Nothing -> []
  agg =
    Aeson.object
      [ "agg"
          .= Aeson.object
            [ "terms" .= Aeson.object ["field" .= term]
            ]
      ]

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
