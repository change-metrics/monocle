{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.RateLimit where

import Data.Morpheus.Client
import Lentille
  ( Log (Log),
    LogAuthor (Macroscope),
    LogEvent (LogRaw),
    MonadGraphQLE,
    MonadLog,
    MonadTime (mThreadDelay),
    mLog,
  )
import Lentille.GraphQL
import Monocle.Logging (LogCrawlerContext)
import Monocle.Prelude
import Network.HTTP.Client (Response, responseBody, responseStatus)
import Network.HTTP.Types (Status, badGateway502, forbidden403, ok200)

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

defineByDocumentFile
  ghSchemaLocation
  [gql|
    query GetRateLimit  {
      rateLimit {
        used
        remaining
        resetAt
      }
    }
  |]

transformResponse :: GetRateLimit -> RateLimit
transformResponse = \case
  GetRateLimit
    ( Just
        (RateLimitRateLimit used remaining (DateTime resetAt'))
      ) -> case parseDateValue $ from resetAt' of
      Just resetAt -> RateLimit {..}
      Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAt'
  respOther -> error ("Invalid response: " <> show respOther)

getRateLimit :: (MonadGraphQLE m) => LogCrawlerContext -> GraphClient -> m RateLimit
getRateLimit lc client = do
  transformResponse
    <$> doRequest client lc mkRateLimitArgs (Just $ retryCheck Macroscope) Nothing Nothing
  where
    mkRateLimitArgs = const . const $ ()

data GHRequestIssue
  = GHRequestTimeout
  | GHRequestSecondaryRateLimit
  | GHRequestRepoNotFound
  | GHRequestUnmatchedIssue Text

retryCheck :: MonadLog m => LogAuthor -> RetryCheck m
retryCheck author = Handler $ \case
  GraphQLError (err, (_req, resp)) -> do
    issueType <- checkResp err resp
    case issueType of
      GHRequestTimeout -> do
        mLog $ Log author $ LogRaw "Server side timeout error. Will retry with lower query depth ..."
        pure True
      GHRequestSecondaryRateLimit -> do
        mLog $ Log author $ LogRaw "Secondary rate limit error. Will retry after 60 seconds ..."
        mThreadDelay $ 60 * 1_000_000
        pure True
      (GHRequestUnmatchedIssue e) -> do
        mLog $ Log author $ LogRaw $ "Unexpected error: " <> e
        pure True
       GHRequestRepoNotFound -> do
        mLog $ Log author $ LogRaw "Repository not found. Will not retry."
        pure False
  where
    checkResp :: (Show a, MonadLog m) => a -> Response LByteString -> m GHRequestIssue
    checkResp err resp = do
      let status = responseStatus resp
          body = decodeUtf8 $ responseBody resp
      if isTimeoutError status body
        then pure GHRequestTimeout
        else
          if isSecondaryRateLimitError status body
            then pure GHRequestSecondaryRateLimit
            else
              if isRepoNotFound status body
                then pure GHRequestRepoNotFound
                else pure $ GHRequestUnmatchedIssue (show err)
    isTimeoutError :: Status -> Text -> Bool
    isTimeoutError status body =
      let msg = "Something went wrong while executing your query. This may be the result of a timeout"
       in status == badGateway502 && inText msg body
    -- https://docs.github.com/en/rest/overview/resources-in-the-rest-api#secondary-rate-limits
    isSecondaryRateLimitError :: Status -> Text -> Bool
    isSecondaryRateLimitError status body =
      let msg = "You have exceeded a secondary rate limit."
       in status == forbidden403 && inText msg body
    isRepoNotFound :: Status -> Text -> Bool
    isRepoNotFound status body =
      let msg = "Could not resolve to a Repository with the name"
       in status == ok200 && inText msg body
