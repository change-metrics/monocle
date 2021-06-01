{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- TMP
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Lentille.GitHub combines the http-client, streaming and morpheus-graphql librarires
--   to implement GitHub graphql API crawlers.
module Lentille.GitHub where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

schemaLocation :: String
schemaLocation = "./github-schema/schema.docs.graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GitHubGraphClient = GitHubGraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    token :: Text
  }

newGithubGraphClient :: MonadIO m => Text -> m GitHubGraphClient
newGithubGraphClient url' = do
  manager' <- liftIO $ HTTP.newManager tlsManagerSettings
  token' <-
    toText
      . fromMaybe (error "GITHUB_GRAPH_TOKEN environment is missing")
      <$> liftIO (lookupEnv "GITHUB_GRAPH_TOKEN")
  pure $ GitHubGraphClient manager' url' token'

-- | The morpheus-graphql-client fetch callback,
-- doc: https://hackage.haskell.org/package/morpheus-graphql-client-0.17.0/docs/Data-Morpheus-Client.html
runGithubGraphRequest :: MonadIO m => GitHubGraphClient -> LBS.ByteString -> m LBS.ByteString
runGithubGraphRequest (GitHubGraphClient manager' url' token') jsonBody = do
  -- putTextLn $ "Sending this query: " <> decodeUtf8 jsonBody
  let initRequest = HTTP.parseRequest_ (toString url')
      request =
        initRequest
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ("Authorization", "token " <> encodeUtf8 token'),
                ("User-Agent", "change-metrics/lentille-morpheus")
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }
  response <- liftIO $ HTTP.httpLbs request manager'
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
  (MonadIO m, Fetch a, FromJSON a) =>
  GitHubGraphClient ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, RateLimit, [Text], [b])) ->
  Stream (Of b) m ()
streamFetch client mkArgs transformResponse = go Nothing
  where
    logStatus (PageInfo hasNextPage' _ totalCount') (RateLimit used' remaining' resetAt') =
      putTextLn $
        "[github-graphql] got "
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
        fetch
          (runGithubGraphRequest client)
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
