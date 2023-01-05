{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.Group where

import Data.Morpheus.Client
import Data.Time.Clock
import Lentille
import Lentille.GitLab.Adapter
import Lentille.GraphQL
import Monocle.Entity (Entity (Organization))
import Monocle.Prelude hiding (break)
import Monocle.Protob.Crawler (Project (..))
import Streaming.Prelude qualified as S

-- https://docs.gitlab.com/ee/api/graphql/reference/#querygroup
declareLocalTypesInline
  glSchemaLocation
  [raw|
    query GetGroupProjects ($fullPath: ID!, $cursor: String) {
      group(fullPath: $fullPath) {
        projects (first: 100, after: $cursor, includeSubgroups: true) {
          pageInfo {hasNextPage endCursor}
          nodes {
            fullPath
          }
        }
      }
    }
  |]

fetchGroupProjects :: GraphEffects es => GraphClient -> Text -> Eff es (Either (FetchError GetGroupProjects) GetGroupProjects, [RequestLog])
fetchGroupProjects client fullPath =
  fetchWithLog (doGraphRequest client) (GetGroupProjectsArgs (ID fullPath) Nothing)

streamGroupProjects :: GraphEffects es => GraphClient -> Text -> LentilleStream es Project
streamGroupProjects client fullPath =
  streamFetch client mkArgs defaultStreamFetchOptParams transformResponse
 where
  mkArgs _ = GetGroupProjectsArgs (ID fullPath)

transformResponse :: GetGroupProjects -> (PageInfo, Maybe RateLimit, [Text], [Project])
transformResponse result =
  case result of
    GetGroupProjects
      ( Just
          ( GetGroupProjectsGroup
              ( GetGroupProjectsGroupProjects
                  (GetGroupProjectsGroupProjectsPageInfo hasNextPage endCursor)
                  nodes
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor Nothing
        , Nothing
        , []
        , getFullPath <$> cleanMaybeMNodes nodes
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing
      , Nothing
      , ["Unknown GetGroupProjects response: " <> show result]
      , []
      )
 where
  getFullPath GetGroupProjectsGroupProjectsNodes {..} = Project . from $ unpackID fullPath
