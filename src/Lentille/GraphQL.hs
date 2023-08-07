{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Helper module to define graphql client
module Lentille.GraphQL (
  -- * client
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

  -- * Some data types
  LentilleStream,
  GraphEffects,
  GraphResponse,
  GraphResp,
  RateLimit (..),
  PageInfo (..),
  StreamFetchOptParams (..),
  defaultStreamFetchOptParams,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Morpheus.Client
import Lentille
import Monocle.Prelude
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI
import Streaming.Prelude qualified as S

import Effectful.Concurrent.MVar qualified as E
import Effectful.Retry
import Monocle.Effects

type GraphEffects es = (LoggerEffect :> es, HttpEffect :> es, PrometheusEffect :> es, TimeEffect :> es, Retry :> es, Concurrent :> es, Fail :> es)

type GraphResponse a = (PageInfo, Maybe RateLimit, [Text], a)

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------
ghSchemaLocation :: FilePath
ghSchemaLocation = "./schemas/github/schema.docs.graphql"

glSchemaLocation :: FilePath
glSchemaLocation = "./schemas/gitlab/schema.graphql"

ghDefaultURL :: Text
ghDefaultURL = "https://api.github.com/graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GraphClient = GraphClient
  { url :: Text
  , host :: Text
  , token :: Secret
  , rateLimitMVar :: MVar (Maybe RateLimit)
  }

newGraphClient ::
  Concurrent :> es =>
  "url" ::: Text ->
  Secret ->
  Eff es GraphClient
newGraphClient url token = do
  rateLimitMVar <- E.newMVar Nothing
  let host =
        maybe
          (error "Unable to parse provided url")
          (from . URI.uriRegName)
          (URI.uriAuthority =<< URI.parseURI (from url))
  pure $ GraphClient {..}

-- | A log of http request and response
type DoFetch es = LBS.ByteString -> WriterT [RequestLog] (Eff es) LBS.ByteString

-- | The morpheus-graphql-client fetch callback,
-- doc: https://hackage.haskell.org/package/morpheus-graphql-client-0.17.0/docs/Data-Morpheus-Client.html
doGraphRequest :: (HttpEffect :> es, PrometheusEffect :> es, LoggerEffect :> es, Retry :> es) => GraphClient -> DoFetch es
doGraphRequest GraphClient {..} jsonBody = do
  -- Prepare the request
  let initRequest = HTTP.parseRequest_ (from url)
      req =
        initRequest
          { HTTP.method = "POST"
          , HTTP.requestHeaders =
              [ bearerTokenHeader token
              , ("User-Agent", "change-metrics/monocle")
              , ("Content-Type", "application/json")
              ]
          , HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }

  -- Do the request (and retry on HttpException raised by the http-client)
  response <- lift (httpRetry url $ httpRequest req)

  -- Record the event
  let responseBody = HTTP.responseBody response
  tell [RequestLog req jsonBody response responseBody]

  -- Return the body so that morpheus run the json decoder
  pure responseBody

-- | Helper function to adapt the morpheus client fetch with a WriterT context
fetchWithLog :: Fetch a => DoFetch es -> Args a -> Eff es (Either (FetchError a) a, [RequestLog])
fetchWithLog cb = runWriterT . fetch cb

-------------------------------------------------------------------------------
-- Streaming layer
-------------------------------------------------------------------------------
data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Maybe Int}
  deriving (Show, Generic, ToJSON)

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: UTCTime}
  deriving (Show, Generic, ToJSON)

instance From RateLimit Text where
  from RateLimit {..} = "remains:" <> show remaining <> ", reset at: " <> show resetAt

type GraphResp a = Either GraphQLError a

-- | wrapper around fetchWithLog than can optionaly handle fetch retries
-- based on the returned data inspection via a provided function (see RetryCheck).
-- In case of retry the depth parameter of mkArgs is decreased (see adaptDepth)
doRequest ::
  forall a es.
  (GraphEffects es, Fetch a, Show a) =>
  GraphClient ->
  (Maybe Int -> Maybe Text -> Args a) ->
  (GraphResp a -> Eff es RetryAction) ->
  Maybe Int ->
  Maybe PageInfo ->
  Eff es (GraphResp a)
doRequest client mkArgs retryCheck depthM pageInfoM =
  retryingDynamic policy (const retryCheck) $ \rs -> do
    when (rs.rsIterNumber > 0)
      $ logWarn "Faulty response" ["num" .= rs.rsIterNumber]
    runFetch rs.rsIterNumber
 where
  delay = 1_100_000 -- 1.1 seconds
  policy = constantDelay delay <> limitRetries 7

  runFetch :: Int -> Eff es (GraphResp a)
  runFetch retried = do
    resp <-
      fetchWithLog
        (doGraphRequest client)
        (mkArgs aDepthM $ (Just . fromMaybe (error "Missing endCursor from page info") . endCursor) =<< pageInfoM)
    pure $ case resp of
      (Right x, _) -> Right x
      -- Throw an exception for the retryCheckM
      (Left e, [req]) -> Left $ GraphQLError (show e) req
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

data StreamFetchOptParams es a = StreamFetchOptParams
  { fpRetryCheck :: GraphResp a -> Eff es RetryAction
  -- ^ Check if the result action needs to be retried
  , fpDepth :: Maybe Int
  -- ^ an optional starting value for the depth
  , fpGetRatelimit :: Maybe (GraphClient -> Eff es (GraphResp (Maybe RateLimit)))
  -- ^ an optional action to get a RateLimit record
  }

defaultStreamFetchOptParams :: StreamFetchOptParams m a
defaultStreamFetchOptParams = StreamFetchOptParams (const $ pure DontRetry) Nothing Nothing

streamFetch ::
  forall es a b.
  (GraphEffects es, Fetch a, Show a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a Maybe depth and a Maybe cursor
  (Maybe Int -> Maybe Text -> Args a) ->
  StreamFetchOptParams es a ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  LentilleStream es b
streamFetch client@GraphClient {..} mkArgs StreamFetchOptParams {..} transformResponse = startFetch
 where
  holdOnIfNeeded :: Maybe RateLimit -> Eff es ()
  holdOnIfNeeded = mapM_ toDelay
   where
    toDelay :: RateLimit -> Eff es ()
    toDelay rl = when (remaining rl <= 0) do
      let resetAtTime = resetAt rl
      logWarn "Reached Quota limit. Waiting until reset date" ["reset" .= resetAtTime]
      holdOnUntil resetAtTime

  requestWithPageInfo pageInfoM storedRateLimitM = do
    holdOnIfNeeded storedRateLimitM
    respE <- doRequest client mkArgs fpRetryCheck fpDepth pageInfoM
    pure $ case respE of
      Left err -> Left err
      Right resp ->
        let (pageInfo, rateLimitM, decodingErrors, xs) = transformResponse resp
         in Right (rateLimitM, (pageInfo, rateLimitM, decodingErrors, xs))

  logStep pageInfo rateLimitM xs totalFetched = do
    lift
      . logInfo "Fetched from current page"
      $ ["count" .= length xs, "total" .= (totalFetched + length xs), "pageInfo" .= pageInfo, "ratelimit" .= rateLimitM]

  retryDelay = 1_100_000

  startFetch = do
    --- Perform a pre GraphQL request to gather rateLimit
    fpRespE <- case fpGetRatelimit of
      Just getRateLimit -> lift
        $ E.modifyMVar rateLimitMVar
        $ const do
          rlE <- getRateLimit client
          case rlE of
            Left err -> do
              logWarn_ "Could not fetch the current rate limit"
              pure (Nothing, Just err)
            Right rl -> pure (rl, Nothing)
      Nothing -> pure Nothing

    case fpRespE of
      Just err -> S.yield (Left $ GraphError err)
      Nothing -> go Nothing 0

  go pageInfoM totalFetched = do
    --- Start by waiting for a few moment if we request a new page
    when (isJust pageInfoM) $ lift $ mThreadDelay retryDelay

    -- Perform the GraphQL request
    respE <- lift $ E.modifyMVar rateLimitMVar $ \rl -> do
      resE <- requestWithPageInfo pageInfoM rl
      pure $ case resE of
        Left err -> (rl, Left err)
        Right (newRL, x) -> (newRL, Right x)

    -- Handle the response
    case respE of
      Left e ->
        -- Yield the error and stop the stream
        S.yield (Left $ GraphError e)
      Right (pageInfo, rateLimitM, decodingErrors, xs) -> do
        -- Log crawling status
        logStep pageInfo rateLimitM xs totalFetched

        case decodingErrors of
          _ : _ -> S.yield (Left $ DecodeError decodingErrors)
          [] -> do
            -- Yield the results
            S.each (Right <$> xs)

            -- Call recursively when response has a next page
            when (hasNextPage pageInfo) $ go (Just pageInfo) (totalFetched + length xs)
