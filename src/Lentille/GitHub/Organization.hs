{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.Organization where

import Data.Morpheus.Client
import Lentille.GitHub.RateLimit (getRateLimit, retryCheck)
import Lentille.GitHub.Types
import Lentille.GraphQL
import Monocle.Prelude
import Monocle.Protob.Crawler (Project (..))

-- https://docs.github.com/en/graphql/reference/objects#organization
declareLocalTypesInline
  ghSchemaLocation
  [raw|
    query GetProjects ($login: String!, $cursor: String)  {
      rateLimit {
        used
        remaining
        resetAt
      }
      organization(login: $login) {
        repositories(isFork: false, first: 100, after: $cursor) {
          totalCount
          pageInfo {hasNextPage endCursor}
          nodes {nameWithOwner}
        }
      }
    }
  |]

transformRateLimit :: GetProjectsRateLimit -> RateLimit
transformRateLimit (GetProjectsRateLimit used remaining (DateTime resetAtText)) =
  case parseDateValue $ from resetAtText of
    Just resetAt -> RateLimit {..}
    Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText

transformResponse :: GetProjects -> (PageInfo, Maybe RateLimit, [Text], [Project])
transformResponse result =
  case result of
    GetProjects
      rateLimitM
      ( Just
          ( GetProjectsOrganization
              ( GetProjectsOrganizationRepositories
                  totalCount
                  (GetProjectsOrganizationRepositoriesPageInfo hasNextPage endCursor)
                  (Just orgRepositories)
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor (Just totalCount)
        , transformRateLimit <$> rateLimitM
        , []
        , getRepos orgRepositories
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing
      , Nothing
      , ["Unknown GetProjects response: " <> show result]
      , []
      )
 where
  getRepos :: [Maybe GetProjectsOrganizationRepositoriesNodes] -> [Project]
  getRepos r = Project . from . nameWithOwner <$> catMaybes r

streamOrganizationProjects :: GraphEffects es => GraphClient -> Text -> LentilleStream es Project
streamOrganizationProjects client login =
  streamFetch client mkArgs optParams transformResponse
 where
  mkArgs _ = GetProjectsArgs login
  optParams =
    defaultStreamFetchOptParams
      { fpRetryCheck = retryCheck
      , fpGetRatelimit = Just getRateLimit
      }
