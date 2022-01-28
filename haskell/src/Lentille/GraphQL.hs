-- | Helper module to define graphql client
module Lentille.GraphQL
  ( -- * client
    newGraphClient,
    GraphClient (host),

    -- * Some cross crawler values
    glSchemaLocation,
    ghSchemaLocation,
    ghDefaultURL,

    -- * Main functions to fetch from a GraphQL server
    doGraphRequest,
    streamFetch,
    fetchWithLog,
    doRequest,

    -- * exception
    GraphQLError (..),

    -- * Some data types
    RateLimit (..),
    PageInfo (..),
    StreamFetchOptParams (..),
    defaultStreamFetchOptParams,

    -- * Some type aliases
    ReqLog,
    RetryCheck,
  )
where

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
-- Exception
-------------------------------------------------------------------------------

-- | GraphQLError is a wrapper around the morpheus's FetchError.
-- TODO: keep the original error data type (instead of the Text)
newtype GraphQLError
  = GraphQLError (Text, (HTTP.Request, HTTP.Response LByteString))
  deriving (Show)

instance Exception GraphQLError

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GraphClient = GraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    host :: Text,
    token :: Secret,
    rateLimitMVar :: MVar (Maybe RateLimit)
  }

newGraphClient ::
  MonadGraphQL m =>
  "url" ::: Text ->
  Secret ->
  m GraphClient
newGraphClient url token = do
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
doGraphRequest :: MonadGraphQL m => LogCrawlerContext -> GraphClient -> DoFetch m
doGraphRequest LogCrawlerContext {..} GraphClient {..} jsonBody = do
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

  -- Do the request (and retry on HttpException raised by the http-client)
  response <- lift $ httpRetry (lccIndex, url, lccName) $ httpRequest request manager

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

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: UTCTime}
  deriving (Show)

instance From RateLimit Text where
  from RateLimit {..} = "remains:" <> show remaining <> ", reset at: " <> show resetAt

-- TODO: find a better name, or remove the type alias.
type RetryCheck m = Handler m Bool

-- | wrapper around fetchWithLog than can optionaly handle fetch retries
-- based on the returned data inspection via a provided function (see RetryCheck).
-- In case of retry the depth parameter of mkArgs is decreased (see adaptDepth)
doRequest ::
  forall a m.
  (MonadGraphQLE m, Fetch a, FromJSON a, Show a) =>
  GraphClient ->
  LogCrawlerContext ->
  (Maybe Int -> Maybe Text -> Args a) ->
  Maybe (RetryCheck m) ->
  Maybe Int ->
  Maybe PageInfo ->
  m a
doRequest client lc mkArgs retryCheckM depthM pageInfoM = retryCheck runFetch
  where
    retryCheck action = case retryCheckM of
      Just rc -> constantRetry retryMessage rc action
      Nothing -> runFetch 0
    -- TODO: Take the retryMessage as a doRequest argument
    retryMessage = "Faulty response - retrying request"
    runFetch :: Int -> m a
    runFetch retried = do
      resp <-
        fetchWithLog
          (doGraphRequest lc client)
          (mkArgs aDepthM $ (Just . fromMaybe (error "Missing endCursor from page info") . endCursor) =<< pageInfoM)
      case resp of
        (Right x, _) -> pure x
        -- Throw an exception for the retryCheckM
        (Left e, [req]) -> throwM $ GraphQLError (show e, req)
        _ -> error $ "Unknown response: " <> show resp
      where
        aDepthM = decreaseValue retried <$> depthM

-- | Slowly decrease a value to workaround api timeout when a graph depth is too deep.
-- >>> decreaseValue 1 42
-- 30
-- >>> decreaseValue 2 42
-- 17
decreaseValue :: Int -> Int -> Int
decreaseValue retried depth =
  let decValue = truncate @Float . (* (fromIntegral retried * 0.3)) . fromIntegral $ depth
   in max 1 $ depth - decValue

data StreamFetchOptParams m a = StreamFetchOptParams
  { -- | an optional retryCheck function
    fpRetryCheck :: Maybe (RetryCheck m),
    -- | an optional starting value for the depth
    fpDepth :: Maybe Int,
    -- | an optional action to get a RateLimit record
    fpGetRatelimit :: Maybe (GraphClient -> m RateLimit)
  }

defaultStreamFetchOptParams :: StreamFetchOptParams m a
defaultStreamFetchOptParams = StreamFetchOptParams Nothing Nothing Nothing

streamFetch ::
  (MonadGraphQLE m, Fetch a, FromJSON a, Show a) =>
  GraphClient ->
  LogCrawlerContext ->
  -- | query Args constructor, the function takes a Maybe depth and a Maybe cursor
  (Maybe Int -> Maybe Text -> Args a) ->
  StreamFetchOptParams m a ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client@GraphClient {..} lc mkArgs StreamFetchOptParams {..} transformResponse = go Nothing 0
  where
    log :: MonadLog m => Text -> m ()
    log = mLog . Log Macroscope . LogGraphQL lc

    holdOnIfNeeded :: (MonadTime m, MonadLog m) => Maybe RateLimit -> m ()
    holdOnIfNeeded = mapM_ toDelay
      where
        toDelay :: (MonadLog m) => RateLimit -> m ()
        toDelay rl = when (remaining rl <= 0) $ do
          let resetAtTime = resetAt rl
          log $ "Reached Quota limit. Waiting until reset date: " <> show resetAtTime
          holdOnUntil resetAtTime

    request pageInfoM storedRateLimitM = do
      holdOnIfNeeded storedRateLimitM
      resp <- doRequest client lc mkArgs fpRetryCheck fpDepth pageInfoM
      let (pageInfo, rateLimitM, decodingErrors, xs) = transformResponse resp
      pure (rateLimitM, (pageInfo, rateLimitM, decodingErrors, xs))

    logStep pageInfo rateLimitM xs totalFetched = do
      lift . log $
        show (length xs)
          <> " doc(s)"
          <> maybe "" (mappend " for " . show) (lccEntity lc)
          <> " fetched from current page (total fetched: "
          <> show (totalFetched + length xs)
          <> ") - "
          <> show pageInfo
          <> " - "
          <> maybe "no ratelimit" show rateLimitM

    retryDelay = 1_100_000

    go pageInfoM totalFetched = do
      --- Start by waiting for a few moment if we request a new page
      when (isJust pageInfoM) $ lift $ mThreadDelay retryDelay

      --- Perform a pre GraphQL request to gather rateLimit
      case fpGetRatelimit of
        Just getRateLimit -> lift $
          mModifyMVar rateLimitMVar $
            const $ do
              rl <- getRateLimit client
              -- Wait few moment to delay the next call
              mThreadDelay retryDelay
              pure (Just rl, ())
        Nothing -> pure ()

      -- Perform the GraphQL request
      (pageInfo, rateLimitM, decodingErrors, xs) <-
        lift $ mModifyMVar rateLimitMVar $ request pageInfoM

      -- Log crawling status
      logStep pageInfo rateLimitM xs totalFetched

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- Call recursively when response has a next page
      when (hasNextPage pageInfo) $ go (Just pageInfo) (totalFetched + length xs)
