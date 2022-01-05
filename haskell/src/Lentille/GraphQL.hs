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

    -- * Utility functions to parse query logs
    handleReqLog,

    -- * Some data types
    RateLimit (..),
    PageInfo (..),

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

handleReqLog :: (MonadThrow m, Show a1) => a1 -> [(HTTP.Request, HTTP.Response LByteString)] -> m a2
handleReqLog err reqLog = case reqLog of
  [(req, resp)] -> throwM $ HttpError (show err, req, resp)
  [] -> error $ "No request log found, error is: " <> show err
  xs -> error $ "Multiple log found for error: " <> show err <> ", " <> show xs

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

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: UTCTime}
  deriving (Show)

instance From RateLimit Text where
  from RateLimit {..} = "remains:" <> show remaining <> ", reset at: " <> show resetAt

type RetryCheck m a = (Either (FetchError a) a, [ReqLog]) -> m Bool

-- | wrapper around fetchWithLog than can optionaly handle fetch retries
-- based on the returned data inspection via a provided function (see RetryCheck).
-- In case of retry the depth parameter of mkArgs is decreased (see adaptDepth)
doRequest ::
  forall a.
  forall m.
  (MonadGraphQLE m, Fetch a, FromJSON a) =>
  GraphClient ->
  (Maybe Int -> Maybe Text -> Args a) ->
  Maybe (RetryCheck m a) ->
  Maybe Int ->
  Maybe PageInfo ->
  m (Either (FetchError a) a, [ReqLog])
doRequest client mkArgs retryCheckM depthM pageInfoM = gRetry retryCheck runFetch
  where
    gRetry = genericRetry Macroscope "Retrying request with smaller depth"
    retryCheck _ = fromMaybe (const $ pure False) retryCheckM
    runFetch :: (MonadGraphQLE m) => Int -> m (Either (FetchError a) a, [ReqLog])
    runFetch retried = do
      let adapedDepth = adaptDepth
      if adapedDepth == Just 0
        then error "Unable to reduce depth more"
        else
          fetchWithLog
            (doGraphRequest client)
            (mkArgs depthM $ (Just . fromMaybe (error "Missing endCursor from page info") . endCursor) =<< pageInfoM)
      where
        adaptDepth :: Maybe Int
        adaptDepth =
          let decValue = truncate @Float . (* (fromIntegral retried * 0.3)) . fromIntegral <$> depthM
           in (-) <$> depthM <*> decValue

streamFetch ::
  (MonadGraphQLE m, Fetch a, FromJSON a, Show a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a Maybe depth and a Maybe cursor
  (Maybe Int -> Maybe Text -> Args a) ->
  -- | an optional retryCheck function
  Maybe (RetryCheck m a) ->
  -- | a starting value for the depth
  Maybe Int ->
  -- | an action to get a RateLimit record
  Maybe (GraphClient -> m RateLimit) ->
  -- | query result adapter
  (a -> (PageInfo, Maybe RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client@GraphClient {..} mkArgs retryCheckM depthM getRateLimitM transformResponse = go Nothing
  where
    log :: MonadLog m => Text -> m ()
    log = mLog . Log Macroscope . LogGraphQL lc
      where
        lc = LogCrawlerContext index crawler

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
      (respE, reqLog) <- doRequest client mkArgs retryCheckM depthM pageInfoM
      (pageInfo, rateLimitM, decodingErrors, xs) <- case respE of
        Left err -> handleReqLog err reqLog
        Right resp -> pure $ transformResponse resp
      pure (rateLimitM, (pageInfo, rateLimitM, decodingErrors, xs))

    go pageInfoM = do
      --- Perform a pre GraphQL request to gather rateLimit
      case getRateLimitM of
        Just getRateLimit -> lift $
          mModifyMVar rateLimitMVar $
            const $ do
              rl <- getRateLimit client
              pure (Just rl, ())
        Nothing -> pure ()

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
