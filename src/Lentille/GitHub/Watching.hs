{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.Watching where

import Data.Morpheus.Client qualified
import Lentille.GitHub.RateLimit qualified
import Lentille.GitHub.Types
import Lentille.GraphQL qualified
import Monocle.Prelude
import Monocle.Protob.Crawler qualified

-- https://docs.github.com/en/graphql/reference/objects#user
Data.Morpheus.Client.declareLocalTypesInline
  Lentille.GraphQL.ghSchemaLocation
  [Data.Morpheus.Client.raw|
    query GetWatched ($login: String!, $cursor: String)  {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $login) {
        watching(last: 100, after: $cursor) {
          totalCount
          pageInfo {hasNextPage endCursor}
          nodes {
            url
          }
        }
      }
    }
  |]

transformResponse :: GetWatched -> Lentille.GraphQL.GraphResponse [Monocle.Protob.Crawler.Project]
transformResponse result = do
  case result of
    GetWatched
      (Just (GetWatchedRateLimit used remaining (DateTime resetAtText)))
      ( Just
          ( GetWatchedUser
              ( GetWatchedUserWatching
                  totalCount
                  (GetWatchedUserWatchingPageInfo hasNextPage endCursor)
                  (Just watchedRepositories)
                )
            )
        ) ->
        let rateLimit = case parseDateValue $ from resetAtText of
              Just resetAt -> Lentille.GraphQL.RateLimit {..}
              Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
         in ( Lentille.GraphQL.PageInfo hasNextPage endCursor (Just totalCount)
            , Just rateLimit
            , Lentille.GraphQL.NoErr
            , getRepos watchedRepositories
            )
    _anyOtherResponse ->
      ( Lentille.GraphQL.PageInfo False Nothing Nothing
      , Nothing
      , Lentille.GraphQL.UnknownErr ["Unknown GetWatched response: " <> show result]
      , []
      )
 where
  getRepos :: [Maybe GetWatchedUserWatchingNodes] -> [Monocle.Protob.Crawler.Project]
  getRepos r = Monocle.Protob.Crawler.Project . from . unURI . url <$> catMaybes r

streamWatchedProjects ::
  Lentille.GraphQL.GraphEffects es =>
  Lentille.GraphQL.GraphClient ->
  Text ->
  Lentille.GraphQL.LentilleStream es Monocle.Protob.Crawler.Project
streamWatchedProjects client login =
  Lentille.GraphQL.streamFetch client mkArgs optParams transformResponse
 where
  mkArgs _ = GetWatchedArgs login
  optParams =
    Lentille.GraphQL.defaultStreamFetchOptParams
      { Lentille.GraphQL.fpRetryCheck = Lentille.GitHub.RateLimit.retryCheck
      , Lentille.GraphQL.fpGetRatelimit = Just Lentille.GitHub.RateLimit.getRateLimit
      }
