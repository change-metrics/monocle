-- | A module for low-level function unavailable in bloodhound
module Database.Bloodhound.Raw (search, aggWithDocValues) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Database.Bloodhound as BH
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as HTTP

dispatch ::
  BH.MonadBH m =>
  HTTP.Method ->
  Text ->
  LByteString ->
  m BH.Reply
dispatch method url body = do
  initReq <- liftIO $ HTTP.parseRequest (toString url)
  let request =
        initReq
          { HTTP.method = method,
            HTTP.requestHeaders =
              ("Content-Type", "application/json") : HTTP.requestHeaders initReq,
            HTTP.requestBody = HTTP.RequestBodyLBS body
          }
  manager <- BH.bhManager <$> BH.getBHEnv
  liftIO $ HTTP.httpLbs request manager

search ::
  (MonadIO m, MonadBH m, MonadThrow m) =>
  (Aeson.ToJSON body, Aeson.FromJSON resp) =>
  BH.IndexName ->
  body ->
  m (BH.SearchResult resp)
search (BH.IndexName index) body = do
  BH.Server s <- BH.bhServer <$> BH.getBHEnv
  let url = Text.intercalate "/" [s, index, "_search"]
      method = HTTP.methodPost
  rawResp <- dispatch method url (Aeson.encode body)
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left _e -> handleError rawResp
    Right x -> pure x
  where
    handleError resp = do
      monocleLog (show resp)
      error "Elastic response failed"

aggWithDocValues :: [(Text, Value)] -> BH.Query -> Value
aggWithDocValues agg query =
  Aeson.object
    [ "aggregations" .= Aeson.object agg,
      "query" .= Aeson.toJSON query,
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
