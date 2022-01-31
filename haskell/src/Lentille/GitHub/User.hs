{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.User where

import Data.Morpheus.Client
import Lentille
import Lentille.GitHub.RateLimit (retryCheck)
import Lentille.GraphQL
import Monocle.Prelude

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

data IdentInfo = IdentInfo
  { iiName :: Maybe Text,
    iiCompany :: Maybe Text
  }

defineByDocumentFile
  ghSchemaLocation
  [gql|
    query getUser ($login: String!) {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $login) {
        name
        company
      }
    }
  |]

transformResponse :: GetUser -> (Maybe RateLimit, IdentInfo)
transformResponse = \case
  GetUser
    (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
    (Just (UserUser name company)) ->
      let rateLimit = case parseDateValue $ from resetAtText of
            Just resetAt -> RateLimit {..}
            Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
       in (Just rateLimit, IdentInfo name company)
  respOther -> error ("Invalid response: " <> show respOther)

getUser :: (MonadGraphQLE m) => LogCrawlerContext -> GraphClient -> Text -> m IdentInfo
getUser lc client login =
  do
    (_, info) <- transformResponse <$> doRequest client lc mkArgs (Just $ retryCheck Macroscope) Nothing Nothing
    pure info
  where
    mkArgs _ _ = GetUserArgs login
