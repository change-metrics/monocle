-- | Helper module to define graphql client
module Lentille.GraphQL where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Lentille
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Streaming.Prelude as S

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------
ghSchemaLocation :: FilePath
ghSchemaLocation = "./github-schema/schema.docs.graphql"

glSchemaLocation :: FilePath
glSchemaLocation = "./gitlab-schema/schema.graphql"

ghDefaultURL :: Text
ghDefaultURL = "https://api.github.com/graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GraphClient = GraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    host :: Text,
    token :: Text
  }

newGraphClient :: MonadGraphQL m => Text -> Text -> m GraphClient
newGraphClient url token = do
  manager <- newManager
  let host =
        maybe
          (error "Unable to parse provided url")
          (toText . URI.uriRegName)
          (URI.uriAuthority =<< URI.parseURI (toString url))
  pure $ GraphClient {..}

-- | A log of http request and response
type ReqLog = (HTTP.Request, HTTP.Response LByteString)

type DoFetch m = LBS.ByteString -> WriterT [ReqLog] m LBS.ByteString

-- | The morpheus-graphql-client fetch callback,
-- doc: https://hackage.haskell.org/package/morpheus-graphql-client-0.17.0/docs/Data-Morpheus-Client.html
doGraphRequest :: MonadGraphQL m => GraphClient -> DoFetch m
doGraphRequest GraphClient {..} jsonBody = do
  -- Prepare the request
  let initRequest = HTTP.parseRequest_ (toString url)
      request =
        initRequest
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ("Authorization", "token " <> encodeUtf8 token),
                ("User-Agent", "change-metrics/monocle"),
                ("Content-Type", "application/json")
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }

  -- Do the request
  response <- lift $ httpRequest request manager

  -- Record the event
  tell [(request, response)]

  -- Return the body so that morpheus run the json decoder
  pure (HTTP.responseBody response)

-- | Helper function to adapt the morpheus client fetch with a WriterT context
fetchWithLog :: (Monad m, FromJSON a, Fetch a) => DoFetch m -> Args a -> m (Either String a, [ReqLog])
fetchWithLog cb = runWriterT . fetch cb

-------------------------------------------------------------------------------
-- Streaming layer
-------------------------------------------------------------------------------
data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Maybe Int}
  deriving (Show)

instance From PageInfo Text where
  from PageInfo {..} = show totalCount <> (if hasNextPage then " hasNextPage" else "")

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: Text}
  deriving (Show)

instance From RateLimit Text where
  from RateLimit {..} = show used <> "/" <> show remaining <> " reset at: " <> resetAt

streamFetch ::
  (MonadGraphQLE m, Fetch a, FromJSON a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client mkArgs transformResponse = go Nothing
  where
    liftLog = lift . logRaw
    logStatus pageInfo rateLimitM =
      liftLog $ "[graphql] got " <> from pageInfo <> " ratelimit " <> maybe "NA" from rateLimitM
    go pageInfoM = do
      (respE, reqLog) <-
        lift $
          fetchWithLog
            (doGraphRequest client)
            (mkArgs . fromMaybe (error "Missing endCursor") $ maybe (Just "") endCursor pageInfoM)

      (pageInfo, rateLimit, decodingErrors, xs) <-
        case respE of
          Left err -> case reqLog of
            [(req, resp)] -> lift $ throwM $ HttpError (from err, req, resp)
            [] -> error $ "No request log found, error is: " <> from err
            xs -> error $ "Multiple log found for error: " <> from err <> ", " <> show xs
          Right resp -> pure $ transformResponse resp

      logStatus pageInfo rateLimit

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- TODO: implement throttle
      when (hasNextPage pageInfo) (go (Just pageInfo))
