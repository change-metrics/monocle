-- | Helper module to define graphql client
module Lentille.GraphQL where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Data.Time (defaultTimeLocale, parseTimeM)
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
    token :: Secret,
    crawler :: Text,
    quotaResetAt :: MVar (Maybe UTCTime)
  }

newGraphClient :: MonadGraphQL m => "crawler" ::: Text -> Text -> Secret -> m GraphClient
newGraphClient crawler url token = do
  manager <- newManager
  quotaResetAt <- initQuotaResetAt Nothing
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
              [ ("Authorization", "token " <> encodeUtf8 (unSecret token)),
                ("User-Agent", "change-metrics/monocle"),
                ("Content-Type", "application/json")
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }

  -- Do the request
  response <- lift $ retry (crawler, url, "crawler") $ httpRequest request manager

  -- Record the event
  tell [(request, response)]

  -- Return the body so that morpheus run the json decoder
  pure (HTTP.responseBody response)

-- | Helper function to adapt the morpheus client fetch with a WriterT context
fetchWithLog :: (Monad m, FromJSON a, Fetch a) => DoFetch m -> Args a -> m (Either (FetchError a) a, [ReqLog])
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
  (MonadGraphQLE m, Fetch a, FromJSON a, Show a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a cursor
  (Maybe Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client mkArgs transformResponse = go Nothing
  where
    log :: MonadLog m => Text -> m ()
    log = mLog . Log Macroscope . LogRaw

    request pageInfoM waitUntilM = do
      case waitUntilM of
        Just waitUntil -> do
          log $ "Reached Quota limit. Waiting until reset date: " <> show waitUntil
          holdOnUntil waitUntil
        Nothing -> pure ()
      (respE, reqLog) <-
        fetchWithLog
          (doGraphRequest client)
          (mkArgs $ (Just . fromMaybe (error "Missing endCursor from page info") . endCursor) =<< pageInfoM)
      (pageInfo, rateLimit, decodingErrors, xs) <- case respE of
        Left err -> case reqLog of
          [(req, resp)] -> throwM $ HttpError (show err, req, resp)
          [] -> error $ "No request log found, error is: " <> show err
          xs -> error $ "Multiple log found for error: " <> show err <> ", " <> show xs
        Right resp -> pure $ transformResponse resp
      let quotaResetAt = case rateLimit of
            Just rateLimit' ->
              if remaining rateLimit' > 0
                then Nothing
                else parseResetAt . resetAt =<< rateLimit
            Nothing -> Nothing
      pure (quotaResetAt, (pageInfo, rateLimit, decodingErrors, xs))

    holdOnUntil :: (MonadTime m) => UTCTime -> m ()
    holdOnUntil resetTime = do
      currentTime <- mGetCurrentTime
      let delaySec = diffUTCTimeToSec resetTime currentTime + 1
      mThreadDelay $ delaySec * 1_000_000
      where
        diffUTCTimeToSec a b =
          truncate (realToFrac . nominalDiffTimeToSeconds $ diffUTCTime a b :: Double) :: Int

    parseResetAt :: Text -> Maybe UTCTime
    parseResetAt epochSText =
      parseTimeM False defaultTimeLocale "%s" (toString epochSText)

    go pageInfoM = do
      -- Perform the GraphQL request
      (pageInfo, rateLimit, decodingErrors, xs) <-
        lift $ withUpdateQuotaResetAt (quotaResetAt client) $ request pageInfoM

      -- Log crawling status
      lift . log $ "[graphql] got " <> from pageInfo <> " ratelimit " <> maybe "NA" from rateLimit

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- TODO: implement throttle
      -- Call recursively when response has a next page
      when (hasNextPage pageInfo) (go (Just pageInfo))
