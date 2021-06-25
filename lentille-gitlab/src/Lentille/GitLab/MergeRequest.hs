{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.MergeRequest (fetchMergeRequest) where

import Data.Morpheus.Client
import Data.Time.Clock
import Lentille.GitLab (GitLabGraphClient, runGitLabGraphRequest, schemaLocation)
import Relude hiding (id)

newtype Time = Time Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- TODO
-- How to fetch comments ?
-- Is adding a commit update the updatedAt time ?

-- Obs
-- Comment added do not update the updatedAt time
-- MergeBy empty in case of bot user
-- Status open

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
  [gql|
    query GetMergeRequest ($project: ID!, $mergeRequestID: String!) {
      project(fullPath: $project) {
      mergeRequest (iid: $mergeRequestID) {
        id
        iid
        title
        description
        webUrl
        commitCount
        diffStatsSummary {
          additions
          deletions
          fileCount
        }
        diffStats {
          path
          additions
          deletions
        }
        commitsWithoutMergeCommits (first: 100) {
          nodes {
            sha
            author {
              username
            }
            authoredDate
            title
          }
        }
        author {
          username
        }
        mergeUser {
          username
        }
        sourceBranch
        targetBranch
        createdAt
        updatedAt
        mergedAt
        mergeable
        labels {
          nodes {
            title
          }
        }
        assignees {
          nodes {
            username
          }
        }
        approved
        draft
      }
      }
    }
  |]

-- client <- newGitLabGraphClient "https://gitlab.com/api/graphql"
-- fetchMergeRequest client "redhat/centos-stream/ci-cd/zuul/jobs" 20
fetchMergeRequest :: MonadIO m => GitLabGraphClient -> Text -> Int -> m (Either String GetMergeRequest)
fetchMergeRequest client project mrID =
  fetch (runGitLabGraphRequest client) (GetMergeRequestArgs (ID project) (show mrID))
