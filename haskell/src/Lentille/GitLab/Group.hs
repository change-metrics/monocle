{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.Group where

import Data.Morpheus.Client
import Data.Time.Clock
import Lentille
import Lentille.GitLab.Adapter
import Lentille.GraphQL
import Monocle.Logging (Entity (Organization), LogCrawlerContext, noContext)
import Monocle.Prelude hiding (break)
import Monocle.Protob.Crawler (Project (..))
import Streaming.Prelude qualified as S

-- https://docs.gitlab.com/ee/api/graphql/reference/#querygroup
defineByDocumentFile
  glSchemaLocation
  [gql|
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

fetchGroupProjects :: MonadGraphQLE m => GraphClient -> Text -> m (Either (FetchError GetGroupProjects) GetGroupProjects, [RequestLog])
fetchGroupProjects client fullPath =
  fetchWithLog (doGraphRequest noContext client) (GetGroupProjectsArgs (ID fullPath) Nothing)

streamGroupProjects ::
  (MonadGraphQLE m) =>
  GraphClient ->
  (Entity -> LogCrawlerContext) ->
  Text ->
  LentilleStream m Project
streamGroupProjects client mkLC fullPath =
  streamFetch client (mkLC $ Organization fullPath) mkArgs defaultStreamFetchOptParams transformResponse
  where
    mkArgs _ = GetGroupProjectsArgs (ID fullPath)

transformResponse :: GetGroupProjects -> (PageInfo, Maybe RateLimit, [Text], [Project])
transformResponse result =
  case result of
    GetGroupProjects
      ( Just
          ( GroupGroup
              ( GroupProjectsProjectConnection
                  (GroupProjectsPageInfoPageInfo hasNextPage endCursor)
                  nodes
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor Nothing,
          Nothing,
          [],
          getFullPath <$> cleanMaybeMNodes nodes
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing,
        Nothing,
        ["Unknown GetGroupProjects response: " <> show result],
        []
      )
  where
    getFullPath GroupProjectsNodesProject {..} = Project . toLazy $ unpackID fullPath
