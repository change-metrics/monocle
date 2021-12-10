{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.Group where

import Data.Morpheus.Client
import Data.Time.Clock
import Lentille
import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GitLab.Adapter
import Lentille.GraphQL
import Monocle.Prelude hiding (break)
import Monocle.Project
import qualified Streaming.Prelude as S

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

fetchGroupProjects :: MonadGraphQLE m => GraphClient -> Text -> m (Either (FetchError GetGroupProjects) GetGroupProjects, [ReqLog])
fetchGroupProjects client fullPath =
  fetchWithLog (doGraphRequest client) (GetGroupProjectsArgs (ID fullPath) Nothing)

streamGroupProjects ::
  (MonadGraphQLE m) =>
  GraphClient ->
  Text ->
  LentilleStream m Project
streamGroupProjects client fullPath = streamFetch client mkArgs (Just getRateLimit) transformResponse
  where
    mkArgs = GetGroupProjectsArgs (ID fullPath)

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
