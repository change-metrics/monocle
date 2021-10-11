-- | A module for low-level function unavailable in bloodhound
module Database.Bloodhound.Raw (ScrollRequest (..), advance, search, searchHit, settings, aggWithDocValues) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Database.Bloodhound as BH
import qualified Json.Extras as Json
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP

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
  initReq <- liftIO $ HTTP.parseRequest (toString url)
  let request =
        initReq
          { HTTP.method = method,
            HTTP.requestHeaders =
              ("Content-Type", "application/json") : HTTP.requestHeaders initReq,
            HTTP.requestBody = HTTP.RequestBodyLBS body
          }
  manager <- BH.bhManager <$> BH.getBHEnv
  liftIO $ HTTP.httpLbs (setQs request) manager
  where
    setQs = case qs of
      [] -> id
      xs -> HTTP.setQueryString xs

-- | Utility function to advance in scroll result. We can use the BH library
--   because we no longer need to support a custom raw body once we have a scroll.
advance :: (MonadIO m, MonadBH m, MonadThrow m, FromJSON resp) => BH.ScrollId -> m (BH.SearchResult resp)
advance scroll = do
  resp <- BH.advanceScroll scroll 60
  case resp of
    Left e -> handleError e
    Right x -> pure x
  where
    handleError resp = do
      monocleLog (show resp)
      error "Elastic scroll response failed"

settings :: (MonadIO m, MonadBH m, ToJSON body) => BH.IndexName -> body -> m ()
settings (BH.IndexName index) body = do
  BH.Server s <- BH.bhServer <$> BH.getBHEnv
  let url = Text.intercalate "/" [s, index, "_settings"]
      method = HTTP.methodPut
  resp <- dispatch method url (Aeson.encode body) []
  case HTTP.responseBody resp of
    "{\"acknowledged\":true}" -> pure ()
    _ -> error $ "Settings apply failed: " <> show resp

search' :: (MonadIO m, MonadBH m, ToJSON body) => BH.IndexName -> body -> QS -> m (BH.Reply)
search' (BH.IndexName index) body qs = do
  BH.Server s <- BH.bhServer <$> BH.getBHEnv
  let url = Text.intercalate "/" [s, index, "_search"]
      method = HTTP.methodPost
  dispatch method url (Aeson.encode body) qs

search ::
  (MonadIO m, MonadBH m, MonadThrow m) =>
  (Aeson.ToJSON body, Aeson.FromJSON resp) =>
  BH.IndexName ->
  body ->
  ScrollRequest ->
  m (BH.SearchResult resp)
search index body scrollRequest = do
  rawResp <- search' index body qs
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left _e -> handleError rawResp
    Right x -> pure x
  where
    qs = case scrollRequest of
      NoScroll -> []
      GetScroll x -> [("scroll", Just x)]
    handleError resp = do
      monocleLog (show resp)
      error "Elastic response failed"

-- | A special purpose search implementation that uses the faster json-syntax
searchHit ::
  (MonadIO m, MonadBH m) =>
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
aggWithDocValues agg query =
  Aeson.object $
    [ "aggregations" .= Aeson.object agg,
      "size"
        .= Aeson.Number 0,
      "docvalue_fields"
        .= Aeson.Array
          ( Vector.fromList
              [ Aeson.object
                  [ "field" .= Aeson.String "created_at",
                    "format" .= Aeson.String "date_time"
                  ]
              ]
          )
    ]
      <> case query of
        Just q -> ["query" .= Aeson.toJSON q]
        Nothing -> []
