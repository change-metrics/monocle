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
import qualified Data.Vector as V
import qualified Google.Protobuf.Timestamp as T
import Lentille.GitLab
  ( GitLabGraphClient,
    PageInfo (..),
    newGitLabGraphClient,
    runGitLabGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Monocle.Change
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

streamMergeRequests :: MonadIO m => GitLabGraphClient -> UTCTime -> String -> Stream (Of Change) m ()
streamMergeRequests client untilDate project =
  streamFetch client untilDate mkArgs transformResponse
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
    nodesToChanges :: [Maybe ProjectMergeRequestsNodesMergeRequest] -> [Change]
    nodesToChanges nodes' =
      map toChange (catMaybes nodes')
      where
        toChange :: ProjectMergeRequestsNodesMergeRequest -> Change
        toChange mr =
          Change
            "Id"
            1
            "ChangeId"
            (toLazy $ title mr)
            "Text"
            "URL"
            1
            0
            0
            0
            changedFiles
            commits
            "rp"
            "rf"
            "rs"
            (Just fakeIdent)
            (Just fakeIdent)
            (Just fakeIdent)
            Nothing
            "my-branch"
            "master"
            Nothing
            Nothing
            Nothing
            (Just $ updatedAtToTimestamp $ updatedAt mr)
            Nothing
            "merged"
            60
            "MERGEABLE"
            labels
            assignees
            approvals
            False
            Nothing
        changedFiles :: V.Vector ChangedFile
        changedFiles = empty
        commits :: V.Vector Commit
        commits = empty
        assignees :: V.Vector Ident
        assignees = empty
        labels :: V.Vector LText
        labels = empty
        approvals :: V.Vector LText
        approvals = empty
        fakeIdent :: Ident
        fakeIdent = Ident "fabien" "fabien"
        updatedAtToTimestamp :: Time -> T.Timestamp
        updatedAtToTimestamp t =
          let Time tt = t in T.fromUTCTime $ parseTimeOrError False defaultTimeLocale "%FT%XZ" $ toString tt
