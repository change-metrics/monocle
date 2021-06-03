{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Relude hiding (id)
import Streaming (Of, Stream)

-- TODO(fbo): use NamedFieldPuns

newtype Time = Time Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

data MergeRequestStateAlt = MergeRequestStateOpened | MergeRequestStateClosed | MergeRequestStateLocked | MergeRequestStateAll | MergeRequestStateMerged

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
  [gql|
    query GetProjectMergeRequests ($project: ID!, $cursor: String) {
      project(fullPath: $project) {
        name
        nameWithNamespace
        namespace {
          name
        }
        mergeRequests (first: 100, after: $cursor, sort: UPDATED_DESC) {
          pageInfo {hasNextPage endCursor}
          count
          nodes {
            id
            title
            description
            webUrl
            commitCount
            diffStatsSummary {
              additions
              deletions
              fileCount
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
              shortName
              fullName
              nameSpaceM
              ( Just
                  ( ProjectMergeRequestsMergeRequestConnection
                      (ProjectMergeRequestsPageInfoPageInfo hasNextPage endCursor)
                      count
                      (Just nodes)
                    )
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor count,
          [],
          (nodesToChanges shortName fullName (toNamespaceName nameSpaceM)) nodes
        )
    otherWise -> error ("Invalid response: " <> show otherwise)
  where
    toNamespaceName nsoM = case nsoM of
      Just nso -> name (nso :: ProjectNamespaceNamespace)
      Nothing -> ""
    fromMTtoLT t = toLazy $ fromMaybe "" t
    fromIntToInt32 = fromInteger . toInteger
    nodesToChanges :: Text -> Text -> Text -> [Maybe ProjectMergeRequestsNodesMergeRequest] -> [Change]
    nodesToChanges shortName fullName namespace nodes' =
      map toChange (catMaybes nodes')
      where
        toChange :: ProjectMergeRequestsNodesMergeRequest -> Change
        toChange mr =
          Change
            (let ID id' = Lentille.GitLab.MergeRequests.id mr in show id')
            1
            "ChangeId"
            (toLazy $ title (mr :: ProjectMergeRequestsNodesMergeRequest))
            (fromMTtoLT $ description mr)
            (fromMTtoLT $ webUrl mr)
            (fromIntToInt32 . fromMaybe 0 $ commitCount mr)
            (getAdditions $ diffStatsSummary mr)
            (getDeletions $ diffStatsSummary mr)
            (getChangedFileCount $ diffStatsSummary mr)
            -- Unable to find a solution for the list of changed files
            empty
            (getCommits $ commitsWithoutMergeCommits mr)
            (toLazy namespace)
            (toLazy fullName)
            (toLazy shortName)
            (Just $ getAuthorIdent mr)
            (Just . ChangeOptionalMergedByMergedBy $ getMergedByIdent mr)
            (toLazy $ sourceBranch mr)
            (toLazy $ targetBranch mr)
            (Just $ timeToTimestamp $ createdAt mr)
            (getMergedAt $ mergedAt mr)
            (Just $ timeToTimestamp $ updatedAt mr)
            -- No closedAt attribute for a MR ?
            Nothing
            "merged"
            60
            "MERGEABLE"
            labels
            assignees
            approvals
            False
            Nothing
        getMergedAt :: Maybe Time -> Maybe ChangeOptionalMergedAt
        getMergedAt tM = fmap (ChangeOptionalMergedAtMergedAt . timeToTimestamp) tM
        ghostIdent = Ident "ghost" "ghost"
        getAuthorIdent :: ProjectMergeRequestsNodesMergeRequest -> Ident
        getAuthorIdent ProjectMergeRequestsNodesMergeRequest {..} = case author of
          Just author' -> toIdent $ getUsername author'
          Nothing -> ghostIdent
          where
            getUsername ProjectMergeRequestsNodesAuthorUserCore {..} = username
        getMergedByIdent :: ProjectMergeRequestsNodesMergeRequest -> Ident
        getMergedByIdent ProjectMergeRequestsNodesMergeRequest {..} = case mergeUser of
          Just author' -> toIdent $ getUsername author'
          Nothing -> ghostIdent
          where
            getUsername ProjectMergeRequestsNodesMergeUserUserCore {..} = username
        getDiffStatsAttr :: Maybe ProjectMergeRequestsNodesDiffStatsSummaryDiffStatsSummary -> (ProjectMergeRequestsNodesDiffStatsSummaryDiffStatsSummary -> Int) -> Int32
        getDiffStatsAttr diffStatsM cb =
          case diffStatsM of
            Just diffStats -> fromIntToInt32 $ cb diffStats
            Nothing -> 0
        getAdditions diffStatsM = getDiffStatsAttr diffStatsM additions
        getDeletions diffStatsM = getDiffStatsAttr diffStatsM deletions
        getChangedFileCount diffStatsM = getDiffStatsAttr diffStatsM fileCount
        getCommits :: Maybe ProjectMergeRequestsNodesCommitsWithoutMergeCommitsCommitConnection -> V.Vector Commit
        getCommits commitsM =
          case commitsM of
            Just
              ( ProjectMergeRequestsNodesCommitsWithoutMergeCommitsCommitConnection
                  (Just commits)
                ) -> fromList $ map toCommit (catMaybes commits)
            _ -> empty
          where
            toCommit :: ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesCommit -> Commit
            toCommit ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesCommit {..} =
              -- Unable to get the committer and the committed_at date
              -- Unable to get additions/deletions
              Commit
                (toLazy sha)
                (Just . toIdent . getUsername $ author)
                (Just . toIdent . getUsername $ author)
                (Just . timeToTimestamp $ fromMaybe defaultTimestamp authoredDate)
                (Just . timeToTimestamp $ fromMaybe defaultTimestamp authoredDate)
                0
                0
                (toLazy $ fromMaybe "" title)
            getUsername :: Maybe ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthorUserCore -> Text
            getUsername uM = case uM of
              Just u -> username (u :: ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthorUserCore)
              Nothing -> ""
            defaultTimestamp = Time "1970-01-01T00:00:00Z"
        toIdent :: Text -> Ident
        toIdent username' = Ident (toLazy $ "gitlab.com" <> "/" <> username') (toLazy username')
        assignees :: V.Vector Ident
        assignees = empty
        labels :: V.Vector LText
        labels = empty
        approvals :: V.Vector LText
        approvals = empty
        timeToTimestamp :: Time -> T.Timestamp
        timeToTimestamp t =
          let Time tt = t in T.fromUTCTime $ parseTimeOrError False defaultTimeLocale "%FT%XZ" $ toString tt
