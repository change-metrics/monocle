{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.MergeRequests where

import Data.Morpheus.Client
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import Lentille.GitLab
  ( GitLabGraphClient,
    PageInfo (..),
    newGitLabGraphClient,
    runGitLabGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Relude
import Streaming (Of, Stream)

newtype Time = Time Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
  [gql|
    query GetProjectMergeRequests ($project: ID!, $cursor: String) {
      project(fullPath: $project) {
        mergeRequests (first: 100, after: $cursor, sort: UPDATED_DESC) {
          pageInfo {hasNextPage endCursor}
          count
          nodes {
            title
            updatedAt
          }
        }
      }
    }
  |]

fetchMergeRequests :: MonadIO m => GitLabGraphClient -> String -> m (Either String GetProjectMergeRequests)
fetchMergeRequests client project =
  fetch (runGitLabGraphRequest client) (GetProjectMergeRequestsArgs (toProjectID project) Nothing)

toProjectID :: String -> ID
toProjectID project' = ID $ toText project'

data Change = Change
  { changeTitle :: Text,
    changeUpdatedAt :: UTCTime
  }
  deriving (Show)

streamMergeRequests :: MonadIO m => GitLabGraphClient -> String -> Stream (Of Change) m ()
streamMergeRequests client project =
  streamFetch client mkArgs transformResponse
  where
    mkArgs cursor = GetProjectMergeRequestsArgs (toProjectID project) $ toCursorM cursor
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just . toString $ cursor''

transformResponse :: GetProjectMergeRequests -> (PageInfo, [Text], [Change])
transformResponse result =
  case result of
    GetProjectMergeRequests
      ( Just
          ( ProjectProject
              ( Just
                  ( ProjectMergeRequestsMergeRequestConnection
                      (ProjectMergeRequestsPageInfoPageInfo hasNextPage endCursor)
                      count
                      (Just nodes)
                    )
                )
            )
        ) -> (PageInfo hasNextPage endCursor count, [], nodesToChanges nodes)
    otherWise -> error ("Invalid response: " <> show otherwise)
  where
    -- fakeChange = Change "A Fake title" getFakeTime
    -- getFakeTime = parseTimeOrError False defaultTimeLocale "%F" "2020-01-01"
    nodesToChanges :: [Maybe ProjectMergeRequestsNodesMergeRequest] -> [Change]
    nodesToChanges nodes' =
      map toChange (catMaybes nodes')
      where
        toChange :: ProjectMergeRequestsNodesMergeRequest -> Change
        toChange mr = Change (title mr) (updatedAtToUTCTime $ updatedAt mr)
        updatedAtToUTCTime :: Time -> UTCTime
        updatedAtToUTCTime t =
          let Time tt = t in parseTimeOrError False defaultTimeLocale "%F" $ toString tt
