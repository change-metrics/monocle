{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.RateLimit where

import Data.Morpheus.Client
import Lentille (MonadGraphQL)
import Lentille.GraphQL
import Monocle.Prelude

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
        (RateLimitRateLimit used remaining (DateTime resetAt))
      ) -> RateLimit {..}
  respOther -> error ("Invalid response: " <> show respOther)

getRateLimit ::
  (MonadGraphQL m) =>
  GraphClient ->
  m (Either (FetchError GetRateLimit) GetRateLimit, [ReqLog])
getRateLimit client = fetchWithLog (doGraphRequest client) ()
