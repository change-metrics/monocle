{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.RateLimit where

import Data.Morpheus.Client
import Lentille
import Lentille.GitHub.Types
import Lentille.GraphQL
import Monocle.Prelude
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types (Status, badGateway502, forbidden403, ok200, unauthorized401)

import Effectful.Retry

declareLocalTypesInline
  ghSchemaLocation
  [raw|
    query GetRateLimit  {
      rateLimit {
        used
        remaining
        resetAt
      }
    }
  |]

transformResponse :: GetRateLimit -> Maybe RateLimit
transformResponse = \case
  GetRateLimit
    ( Just
        (GetRateLimitRateLimit used remaining (DateTime resetAt'))
      ) -> case parseDateValue $ from resetAt' of
      Just resetAt -> Just RateLimit {..}
      Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAt'
  GetRateLimit Nothing -> Nothing
  respOther -> error ("Invalid response: " <> show respOther)

getRateLimit :: GraphEffects es => GraphClient -> Eff es (Either GraphQLError (Maybe RateLimit))
getRateLimit client = do
  fmap transformResponse
    <$> doRequest client mkRateLimitArgs retryCheck Nothing Nothing
 where
  mkRateLimitArgs = const . const $ ()

retryCheck :: forall es a. GraphEffects es => Either GraphQLError a -> Eff es RetryAction
retryCheck = \case
  Right _ -> pure DontRetry
  Left (GraphQLError err (RequestLog _ _ resp _))
    | status == unauthorized401 -> do
        logWarn "Authentication error" ["body" .= body]
        pure DontRetry
    | isTimeoutError status body -> do
        logWarn_ "Server side timeout error. Will retry with lower query depth ..."
        pure ConsultPolicy
    | isSecondaryRateLimitError status body -> do
        logWarn_ "Secondary rate limit error. Will retry after 60 seconds ..."
        pure (ConsultPolicyOverrideDelay $ 60 * 1_000_000)
    | isRepoNotFound status body -> do
        logWarn_ "Repository not found. Will not retry."
        pure DontRetry
    | otherwise -> do
        logWarn "Unexpected error" ["err" .= show @Text err]
        pure ConsultPolicy
   where
    status = responseStatus resp
    body = decodeUtf8 $ responseBody resp
 where
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
