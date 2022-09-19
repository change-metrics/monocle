{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.Organization where

import Data.Morpheus.Client
import Lentille (MonadGraphQLE)
import Lentille.GitHub.RateLimit (getRateLimit, retryCheck)
import Lentille.GraphQL
import Monocle.Entity (Entity (Organization))
import Monocle.Logging
import Monocle.Prelude
import Monocle.Protob.Crawler (Project (..))

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.github.com/en/graphql/reference/objects#organization
defineByDocumentFile
  ghSchemaLocation
  [gql|
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

transformResponse :: GetProjects -> (PageInfo, Maybe RateLimit, [Text], [Project])
transformResponse result = do
  case result of
    GetProjects
      (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
      ( Just
          ( OrganizationOrganization
              ( OrganizationRepositoriesRepositoryConnection
                  totalCount
                  (OrganizationRepositoriesPageInfoPageInfo hasNextPage endCursor)
                  (Just orgRepositories)
                )
            )
        ) ->
        let rateLimit = case parseDateValue $ from resetAtText of
              Just resetAt -> RateLimit {..}
              Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
         in ( PageInfo hasNextPage endCursor (Just totalCount)
            , Just rateLimit
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
  getRepos :: [Maybe OrganizationRepositoriesNodesRepository] -> [Project]
  getRepos r = Project . from . nameWithOwner <$> catMaybes r

streamOrganizationProjects :: (HasLogger m, MonadGraphQLE m) => GraphClient -> (Entity -> LogCrawlerContext) -> Text -> Stream (Of Project) m ()
streamOrganizationProjects client mkLC login =
  streamFetch client lc mkArgs optParams transformResponse
 where
  lc = mkLC $ Organization login
  mkArgs _ = GetProjectsArgs login
  optParams =
    defaultStreamFetchOptParams
      { fpRetryCheck = Just retryCheck
      , fpGetRatelimit = Just $ getRateLimit lc
      }
