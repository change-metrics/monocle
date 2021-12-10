{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Get user's favorites:
-- > $ cabal repl
-- > λ> import Lentille.GitHub.Favorites
-- > λ> xs <- S.toList_ $ (newGithubGraphClient "https://api.github.com/graphql" >>= \client -> getFavoritesStream client "TristanCacqueray")
-- > [github-graphql] got 1460 hasNextPage  ratelimit 1/4999 reset at: 2021-05-04T00:00:17Z
-- > ...
-- > λ> xs
-- > [ UserStarredRepositoriesEdgesNodeRepository {
--       nameWithOwner = "morpheusgraphql/morpheus-graphql",
--       stargazerCount = 298,
--       updatedAt = DateTime "2021-05-03T21:00:22Z",
--       description = Just "Haskell GraphQL Api, Client and Tools"
--     },
-- > , ...
-- > ]
module Lentille.GitHub.Favorites where

import Data.Morpheus.Client
import Lentille (MonadGraphQLE)
import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GraphQL
import Monocle.Prelude

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

defineByDocumentFile
  ghSchemaLocation
  [gql|
    query GetFavorites ($userName: String!, $cursor: String)
    {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $userName) {
        starredRepositories(
          first: 100,
          after: $cursor,
          orderBy: {direction: ASC, field: STARRED_AT}
        ) {
          totalCount
          pageInfo {hasNextPage endCursor}
          edges {
            node {
              nameWithOwner
              stargazerCount
              updatedAt
              description
            }
          }
        }
      }
    }
  |]

type UserFavorite = UserStarredRepositoriesEdgesNodeRepository

getFavoritesStream ::
  MonadGraphQLE m =>
  GraphClient ->
  Text ->
  Stream (Of UserFavorite) m ()
getFavoritesStream client username = streamFetch client mkArgs (Just getRateLimit) transformResponse
  where
    mkArgs = GetFavoritesArgs username
    transformResponse :: GetFavorites -> (PageInfo, Maybe RateLimit, [Text], [UserFavorite])
    transformResponse resp = case resp of
      GetFavorites
        (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
        ( Just
            ( UserUser
                ( UserStarredRepositoriesStarredRepositoryConnection
                    totalCount'
                    (UserStarredRepositoriesPageInfoPageInfo hasNextPage' endCursor')
                    (Just xs)
                  )
              )
          ) ->
          let rateLimit = case parseDateValue $ from resetAtText of
                Just resetAt -> RateLimit {..}
                Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
           in ( PageInfo hasNextPage' endCursor' (Just totalCount'),
                Just rateLimit,
                [],
                map getNode $ catMaybes xs
              )
      respOther -> error ("Invalid response: " <> show respOther)
    getNode (UserStarredRepositoriesEdgesStarredRepositoryEdge node') = node'
