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
    index :: Text,
    rateLimitMVar :: MVar (Maybe RateLimit)
  }

newGraphClient ::
  MonadGraphQL m =>
  "indexName" ::: Text ->
  "crawler" ::: Text ->
  "url" ::: Text ->
  Secret ->
  m GraphClient
newGraphClient index crawler url token = do
  manager <- newManager
  rateLimitMVar <- mNewMVar Nothing
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
  from PageInfo {..} =
    "total docs: "
      <> maybe "" show totalCount
      <> (if hasNextPage then " (has next page)" else "")

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: Text}
  deriving (Show)

instance From RateLimit Text where
  from RateLimit {..} = "remains:" <> show remaining <> ", reset at: " <> resetAt

streamFetch ::
  (MonadGraphQLE m, Fetch a, FromJSON a, Show a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a cursor
  (Maybe Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client@GraphClient {..} mkArgs transformResponse = go Nothing
  where
    log :: MonadLog m => Text -> m ()
    log = mLog . Log Macroscope . LogGraphQL lc
      where
        lc = LogCrawlerContext index crawler

    holdOnIfNeeded :: (MonadTime m, MonadLog m) => Maybe RateLimit -> m ()
    holdOnIfNeeded = \case
      Nothing -> pure ()
      Just rl -> do
        if remaining rl <= 0
          then do
            case parseResetAt $ resetAt rl of
              Just waitDelay -> do
                log $ "Reached Quota limit. Waiting until reset date: " <> show waitDelay
                holdOnUntil waitDelay
              Nothing -> do
                error $ "Unable to parse the resetAt date: " <> resetAt rl
          else pure ()
        pure ()
      where
        holdOnUntil :: (MonadTime m) => UTCTime -> m ()
        holdOnUntil resetTime = do
          currentTime <- mGetCurrentTime
          let delaySec = diffUTCTimeToSec resetTime currentTime + 1
          mThreadDelay $ delaySec * 1_000_000
          where
            diffUTCTimeToSec a b =
              truncate (realToFrac . nominalDiffTimeToSeconds $ diffUTCTime a b :: Double) :: Int
        parseResetAt :: Text -> Maybe UTCTime
        parseResetAt dateT =
          parseTimeM False defaultTimeLocale "%FT%XZ" $ from dateT

    request pageInfoM storedRateLimitM = do
      holdOnIfNeeded storedRateLimitM
      (respE, reqLog) <-
        fetchWithLog
          (doGraphRequest client)
          (mkArgs $ (Just . fromMaybe (error "Missing endCursor from page info") . endCursor) =<< pageInfoM)
      (pageInfo, rateLimitM, decodingErrors, xs) <- case respE of
        Left err -> case reqLog of
          [(req, resp)] -> throwM $ HttpError (show err, req, resp)
          [] -> error $ "No request log found, error is: " <> show err
          xs -> error $ "Multiple log found for error: " <> show err <> ", " <> show xs
        Right resp -> pure $ transformResponse resp
      pure (rateLimitM, (pageInfo, rateLimitM, decodingErrors, xs))

    go pageInfoM = do
      -- Perform the GraphQL request
      (pageInfo, rateLimitM, decodingErrors, xs) <-
        lift $ mModifyMVar rateLimitMVar $ request pageInfoM

      -- Log crawling status
      lift . log $ "graphQL client infos: " <> from pageInfo <> " ratelimit " <> maybe "NA" from rateLimitM

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- TODO: implement throttle
      -- Call recursively when response has a next page
      when (hasNextPage pageInfo) (go (Just pageInfo))
