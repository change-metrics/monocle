-- | Helper module to define graphql client
module Lentille.GraphQL where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Lentille
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Streaming.Prelude as S

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------
ghSchemaLocation :: FilePath
ghSchemaLocation = "./github-schema/schema.docs.graphql"

ghDefaultURL :: Text
ghDefaultURL = "https://api.github.com/graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GraphClient = GraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    token :: Text
  }

newGraphClient :: MonadGraphQL m => Text -> Text -> m GraphClient
newGraphClient url token = do
  manager <- newManager
  pure $ GraphClient {..}

-- | The morpheus-graphql-client fetch callback,
-- doc: https://hackage.haskell.org/package/morpheus-graphql-client-0.17.0/docs/Data-Morpheus-Client.html
doGraphRequest :: MonadGraphQL m => GraphClient -> LBS.ByteString -> m LBS.ByteString
doGraphRequest GraphClient {..} jsonBody = do
  -- putTextLn $ "Sending this query: " <> decodeUtf8 jsonBody
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
  response <- httpRequest request manager
  -- print response
  pure (HTTP.responseBody response)

-------------------------------------------------------------------------------
-- Streaming layer
-------------------------------------------------------------------------------
data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Int}
  deriving (Show)

data RateLimit = RateLimit {used :: Int, remaining :: Int, resetAt :: Text}
  deriving (Show)

streamFetch ::
  (MonadGraphQL m, Fetch a, FromJSON a) =>
  GraphClient ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client mkArgs transformResponse = go Nothing
  where
    liftLog = lift . logRaw
    logStatus (PageInfo hasNextPage' _ totalCount') (RateLimit used' remaining' resetAt') =
      liftLog $
        "[graphql] got "
          <> show totalCount'
          <> (if hasNextPage' then " hasNextPage " else "")
          <> " ratelimit "
          <> show used'
          <> "/"
          <> show remaining'
          <> " reset at: "
          <> resetAt'
    go pageInfoM = do
      respE <-
        lift $
          fetch
            (doGraphRequest client)
            (mkArgs . fromMaybe (error "Missing endCursor") $ maybe (Just "") endCursor pageInfoM)
      let (pageInfo, rateLimit, decodingErrors, xs) = case respE of
            Left err -> error (toText err)
            Right resp -> transformResponse resp
      -- TODO: report decoding error
      unless (null decodingErrors) (error ("Decoding failed: " <> show decodingErrors))
      logStatus pageInfo rateLimit
      S.each xs
      -- TODO: implement throttle
      when (hasNextPage pageInfo) (go (Just pageInfo))
