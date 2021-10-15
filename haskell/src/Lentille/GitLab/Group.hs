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
import Lentille.GitLab
  ( GitLabGraphClient,
    PageInfo (..),
    host,
    newGitLabGraphClient,
    runGitLabGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Lentille.GitLab.Adapter
import Monocle.Prelude hiding (break)
import Monocle.Project
import qualified Streaming.Prelude as S

-- https://docs.gitlab.com/ee/api/graphql/reference/#querygroup
defineByDocumentFile
  schemaLocation
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

fetchGroupProjects :: MonadGraphQL m => GitLabGraphClient -> Text -> m (Either String GetGroupProjects)
fetchGroupProjects client fullPath =
  fetch (runGitLabGraphRequest client) (GetGroupProjectsArgs (ID fullPath) Nothing)

streamGroupProjects ::
  (MonadError LentilleError m, MonadGraphQL m) =>
  GitLabGraphClient ->
  Text ->
  LentilleStream m Project
streamGroupProjects client fullPath =
  streamFetch client Nothing mkArgs transformResponse id
  where
    mkArgs cursor = GetGroupProjectsArgs (ID fullPath) $ toCursorM cursor
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just . toString $ cursor''

transformResponse :: GetGroupProjects -> (PageInfo, [Text], [Project])
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
          [],
          getFullPath <$> cleanMaybeMNodes nodes
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing,
        ["Unknown GetGroupProjects response: " <> show result],
        []
      )
  where
    getFullPath GroupProjectsNodesProject {..} = Project . toLazy $ unpackID fullPath
