{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- TMP
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lentille.GitLab where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Data.Time.Clock
import Monocle.Api.Client.Worker (mkManager)
import qualified Network.HTTP.Client as HTTP
import Relude
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

schemaLocation :: String
schemaLocation = "./gitlab-schema/schema.graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GitLabGraphClient = GitLabGraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    token :: Text
  }

newGitLabGraphClientWithKey :: MonadIO m => Text -> Text -> m GitLabGraphClient
newGitLabGraphClientWithKey url' token' = do
  manager' <- mkManager
  pure $ GitLabGraphClient manager' url' token'

newGitLabGraphClient :: MonadIO m => Text -> m GitLabGraphClient
newGitLabGraphClient url' = do
  token' <-
    toText
      . fromMaybe (error "GITLAB_GRAPH_TOKEN environment is missing")
      <$> liftIO (lookupEnv "GITLAB_GRAPH_TOKEN")
  newGitLabGraphClientWithKey url' token'

runGitLabGraphRequest :: MonadIO m => GitLabGraphClient -> LBS.ByteString -> m LBS.ByteString
runGitLabGraphRequest (GitLabGraphClient manager' url' token') jsonBody = do
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
  response <- liftIO $ HTTP.httpLbs request manager'
  -- print response
  pure (HTTP.responseBody response)

data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Int}
  deriving (Show)

streamFetch ::
  (MonadIO m, Fetch a, FromJSON a) =>
  GitLabGraphClient ->
  -- | MR updatedAt date until we need to fetch
  UTCTime ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, [Text], [b])) ->
  -- | check for limit ->
  (Stream (Of b) m () -> Stream (Of b) m ()) ->
  Stream (Of b) m ()
streamFetch client untilDate mkArgs transformResponse checkLimit = go Nothing
  where
    logStatus (PageInfo hasNextPage' _ totalCount') =
      putTextLn $
        "[gitlab-graphql] got total count of MR: "
          <> show totalCount'
          <> " fetching until date: "
          <> show untilDate
          <> (if hasNextPage' then " [hasNextPage] " else "")
    go pageInfoM = do
      -- Get current page results
      respE <-
        fetch
          (runGitLabGraphRequest client)
          (mkArgs . fromMaybe (error "Missing endCursor") $ maybe (Just "") endCursor pageInfoM)
      let (pageInfo, decodingErrors, xs) = case respE of
            Left err -> error (toText err)
            Right resp -> transformResponse resp

      -- TODO: report decoding error
      unless (null decodingErrors) (error ("Decoding failed: " <> show decodingErrors))

      logStatus pageInfo

      -- Yield the results
      checkLimit (S.each xs)

      -- TODO: implement throttle
      when (hasNextPage pageInfo) (go (Just pageInfo))
