{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.RateLimit where

import Data.Morpheus.Client
import Lentille
  ( LentilleError (GraphQLError),
    Log (Log),
    LogAuthor (Macroscope),
    LogEvent (LogRaw),
    MonadGraphQLE,
    MonadLog,
    MonadTime (mThreadDelay),
    RequestLog (..),
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

data RetryResult
  = DoRetry
  | DontRetry

retryResultToBool :: RetryResult -> Bool
retryResultToBool DoRetry = True
retryResultToBool DontRetry = False

retryCheck :: MonadLog m => LogAuthor -> RetryCheck m
retryCheck author = Handler $ \case
  GraphQLError (err, RequestLog _req _body resp _rbody) -> retryResultToBool <$> checkResp err resp
  _anyOtherExceptionAreNotRetried -> pure False
  where
    checkResp :: (Show a, MonadLog m) => a -> Response LByteString -> m RetryResult
    checkResp err resp
      | isTimeoutError status body = do
        mLog $ Log author $ LogRaw "Server side timeout error. Will retry with lower query depth ..."
        pure DoRetry
      | isSecondaryRateLimitError status body = do
        mLog $ Log author $ LogRaw "Secondary rate limit error. Will retry after 60 seconds ..."
        mThreadDelay $ 60 * 1_000_000
        pure DoRetry
      | isRepoNotFound status body = do
        mLog $ Log author $ LogRaw "Repository not found. Will not retry."
        pure DontRetry
      | otherwise = do
        mLog $ Log author $ LogRaw $ "Unexpected error: " <> show err
        pure DoRetry
      where
        status = responseStatus resp
        body = decodeUtf8 $ responseBody resp

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
