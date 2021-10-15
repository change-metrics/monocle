{-# LANGUAGE FlexibleContexts #-}
-- TMP
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lentille.GitLab where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Data.Time.Clock
import Lentille
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import qualified Streaming.Prelude as S

schemaLocation :: String
schemaLocation = "./gitlab-schema/schema.graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GitLabGraphClient = GitLabGraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    token :: Text,
    host :: Text
  }

newGitLabGraphClientWithKey :: MonadGraphQL m => Text -> Text -> m GitLabGraphClient
newGitLabGraphClientWithKey url' token' = do
  manager' <- newManager
  let host' =
        maybe
          (error "Unable to parse provided gitlab_url")
          (toText . URI.uriRegName)
          (URI.uriAuthority =<< URI.parseURI (toString url'))
  pure $ GitLabGraphClient manager' url' token' host'

newGitLabGraphClient :: MonadGraphQL m => Text -> m GitLabGraphClient
newGitLabGraphClient url' = do
  token' <-
    toText
      . fromMaybe (error "GITLAB_GRAPH_TOKEN environment is missing")
      <$> mLookupEnv "GITLAB_GRAPH_TOKEN"
  newGitLabGraphClientWithKey url' token'

runGitLabGraphRequest :: MonadGraphQL m => GitLabGraphClient -> LBS.ByteString -> m LBS.ByteString
runGitLabGraphRequest (GitLabGraphClient manager' url' token' _) jsonBody = do
  -- putTextLn $ "Sending this query: " <> decodeUtf8 jsonBody
  let initRequest = HTTP.parseRequest_ (toString url')
      request =
        initRequest
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ("Authorization", "Bearer " <> encodeUtf8 token'),
                ("User-Agent", "change-metrics/lentille-morpheus"),
                ("Content-Type", "application/json")
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }
  response <- httpRequest request manager'
  -- print response
  pure $ HTTP.responseBody response

data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Maybe Int}
  deriving (Show)

streamFetch ::
  (Fetch a, FromJSON a, MonadGraphQL m) =>
  GitLabGraphClient ->
  -- | MR updatedAt date until we need to fetch
  Maybe UTCTime ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, [Text], [b])) ->
  -- | check for limit ->
  (LentilleStream m b -> LentilleStream m b) ->
  LentilleStream m b
streamFetch client untilDate mkArgs transformResponse checkLimit = checkLimit $ go Nothing
  where
    liftLog = lift . logRaw
    logStatus (PageInfo hasNextPage' _ totalCount') =
      liftLog $
        "[gitlab-graphql] got total count of documents: "
          <> show totalCount'
          <> " fetching until date: "
          <> show untilDate
          <> (if hasNextPage' then " [hasNextPage] " else "")
    go pageInfoM = do
      -- Get current page results
      respE <-
        lift
          . retry
          $ fetch
            (runGitLabGraphRequest client)
            (mkArgs . fromMaybe (error "Missing endCursor") $ maybe (Just "") endCursor pageInfoM)
      let (pageInfo, decodingErrors, xs) = case respE of
            Left err -> error (toText err)
            Right resp -> transformResponse resp

      logStatus pageInfo

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- TODO: implement throttle
      when (hasNextPage pageInfo) (go (Just pageInfo))
