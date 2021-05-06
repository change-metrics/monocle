{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitHub.Issues where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille.GitHub
  ( GitHubGraphClient,
    PageInfo (..),
    RateLimit (..),
    runGithubGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Monocle.TaskData
import Relude
import Streaming (Of, Stream)

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

defineByDocumentFile
  schemaLocation
  [gql|
    query GetLinkedIssues ($search: String!, $cursor: String) {
      rateLimit {
        used
        remaining
        resetAt
      }
      search(query: $search, type: ISSUE, first: 25, after: $cursor) {
        issueCount
        pageInfo {hasNextPage endCursor}
        nodes {
          ... on Issue {
            id
            title
            updatedAt
            url
            labels(first: 100) {
              nodes {
                name
              }
            }
            timelineItems(first: 100, itemTypes: [CONNECTED_EVENT]) {
              nodes {
                ... on ConnectedEvent {
                  subject {
                    ... on PullRequest {url}
                  }
                }
              }
            }
          }
        }
      }
    }
  |]

-- fetchLinkedIssue :: MonadIO m => GitHubGraphClient -> String -> m (Either String GetLinkedIssues)
-- fetchLinkedIssue client searchText = fetch (runGithubGraphRequest client) (GetLinkedIssuesArgs searchText "")

streamLinkedIssue :: MonadIO m => GitHubGraphClient -> String -> Stream (Of NewTaskData) m ()
streamLinkedIssue client searchText =
  streamFetch client mkArgs transformResponse
  where
    mkArgs cursor = GetLinkedIssuesArgs searchText $ toCursorM cursor
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just . toString $ cursor''

transformResponse :: GetLinkedIssues -> (PageInfo, RateLimit, [NewTaskData])
transformResponse searchResult =
  case searchResult of
    GetLinkedIssues
      (Just (RateLimitRateLimit used' remaining' (DateTime resetAt')))
      ( SearchSearchResultItemConnection
          issueCount
          (SearchPageInfoPageInfo hasNextPage' endCursor')
          (Just issues)
        ) ->
        ( PageInfo hasNextPage' endCursor' issueCount,
          RateLimit used' remaining' resetAt',
          (concatMap mkNewTaskData issues)
        )
    respOther -> error ("Invalid response: " <> show respOther)
  where
    mkNewTaskData :: Maybe SearchNodesSearchResultItem -> [NewTaskData]
    mkNewTaskData issueM = case issueM of
      Just issue -> map (toNewTaskData issue) (getTDChangeUrls issue)
      Nothing -> []
    toNewTaskData :: SearchNodesSearchResultItem -> Text -> NewTaskData
    toNewTaskData issue curl =
      NewTaskData
        (Just $ getUpdatedAt issue)
        (toLazy curl)
        (toLazy <$> V.fromList (getLabels issue))
        (toLazy $ getIssueID issue)
        (toLazy $ getIssueURL issue)
        (toLazy $ title issue)
        "low"
        "low"
        0
      where
        getIssueURL :: SearchNodesSearchResultItem -> Text
        getIssueURL (SearchNodesIssue _ _ _ changeURL _ _) = show changeURL
        getIssueID :: SearchNodesSearchResultItem -> Text
        getIssueID (SearchNodesIssue issueID _ _ _ _ _) = unpackID issueID
    getUpdatedAt :: SearchNodesSearchResultItem -> Timestamp
    getUpdatedAt (SearchNodesIssue _ _ (DateTime updatedAt) _ _ _) =
      fromMaybe
        (error "Unable to decode updatedAt format")
        (Timestamp.fromText updatedAt)
    getLabels :: SearchNodesSearchResultItem -> [Text]
    getLabels issue =
      case issue of
        SearchNodesIssue
          _
          _
          _
          _
          (Just (SearchNodesLabelsLabelConnection (Just nodesLabel)))
          _ -> map getLabelFromNode nodesLabel
        respOther -> error ("Invalid response: " <> show respOther)
    getLabelFromNode :: Maybe SearchNodesLabelsNodesLabel -> Text
    getLabelFromNode nodeLabelM = case nodeLabelM of
      Just
        (SearchNodesLabelsNodesLabel (label)) -> label
      Nothing -> error ("Missing Label in SearchNodesLabelsNodesLabel")
    getTDChangeUrls :: SearchNodesSearchResultItem -> [Text]
    getTDChangeUrls issue =
      case issue of
        SearchNodesIssue
          _
          _
          _
          _
          _
          ( SearchNodesTimelineItemsIssueTimelineItemsConnection
              (Just urls)
            ) -> map extractUrl urls
        respOther -> error ("Invalid response: " <> show respOther)
    extractUrl :: Maybe SearchNodesTimelineItemsNodesIssueTimelineItems -> Text
    extractUrl item = case item of
      Just
        ( SearchNodesTimelineItemsNodesConnectedEvent
            (SearchNodesTimelineItemsNodesSubjectPullRequest (url))
          ) -> show url
      -- We are requesting Issue with connected PR we cannot get Nothing
      Nothing -> error ("Missing PR URI in SearchNodesTimelineItemsNodesSubjectPullRequest")
