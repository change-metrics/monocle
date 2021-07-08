{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitHub.Issues where

import Data.Aeson (decode)
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

-- The query muste use the filter "linked:pr" filter. steamLinkedIssue ensures this.
-- CONNECTED_EVENT happens when a LINK is performed manually though the GH UI.
-- CROSS_CONNECTED_EVENT happens when a PR reference an issue in some way
--  Most of the time this is used by developers to reference an issue in
--  the PR description or in a commit messages.
-- See GH documentation https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
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
            timelineItems(first: 100, itemTypes: [CONNECTED_EVENT, CROSS_REFERENCED_EVENT]) {
              nodes {
                __typename
                ... on CrossReferencedEvent {
                   source {
                      ... on PullRequest {url}
                   }
                }
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

streamLinkedIssue :: MonadIO m => GitHubGraphClient -> String -> Stream (Of TaskData) m ()
streamLinkedIssue client searchText =
  streamFetch client mkArgs transformResponse
  where
    mkArgs cursor' = GetLinkedIssuesArgs (searchText <> " linked:pr") $ toCursorM cursor'
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just $ toString cursor''

pattern IssueLabels nodesLabel <- SearchNodesIssue _ _ _ _ (Just (SearchNodesLabelsLabelConnection (Just nodesLabel))) _

transformResponse :: GetLinkedIssues -> (PageInfo, RateLimit, [Text], [TaskData])
transformResponse searchResult =
  case searchResult of
    GetLinkedIssues
      (Just (RateLimitRateLimit used' remaining' (DateTime resetAt')))
      ( SearchSearchResultItemConnection
          issueCount'
          (SearchPageInfoPageInfo hasNextPage' endCursor')
          (Just issues)
        ) ->
        let newTaskDataE = concatMap mkTaskData issues
         in ( PageInfo hasNextPage' endCursor' issueCount',
              RateLimit used' remaining' resetAt',
              lefts newTaskDataE,
              rights newTaskDataE
            )
    respOther -> error ("Invalid response: " <> show respOther)
  where
    mkTaskData :: Maybe SearchNodesSearchResultItem -> [Either Text TaskData]
    mkTaskData issueM = case issueM of
      Just issue ->
        -- (fmap . join . fmap $ toTaskData issue) (getTDChangeUrls issue)
        let tdChangeUrlsE :: [Either Text Text]
            tdChangeUrlsE = getTDChangeUrls issue
            newTaskDataEE :: [Either Text (Either Text TaskData)]
            newTaskDataEE = (fmap . fmap $ toTaskData issue) tdChangeUrlsE
            newTaskDataE = fmap join newTaskDataEE
         in newTaskDataE
      Nothing -> []
    toTaskData :: SearchNodesSearchResultItem -> Text -> Either Text TaskData
    toTaskData issue curl =
      TaskData
        <$> (Just <$> getUpdatedAt issue)
        <*> pure (toLazy curl)
        <*> (V.fromList <$> getLabelsE)
        <*> pure (toLazy $ getIssueID issue)
        <*> pure (toLazy $ getIssueURL issue)
        <*> pure (toLazy $ title issue)
        <*> pure "low"
        <*> pure "low"
        <*> pure 0
      where
        getLabelsE :: Either Text [LText]
        getLabelsE = case partitionEithers (getLabels issue) of
          (labels', []) -> Right (fmap toLazy labels')
          (_, errors) -> Left (unwords errors)
        getIssueURL :: SearchNodesSearchResultItem -> Text
        getIssueURL (SearchNodesIssue _ _ _ (URI changeURL) _ _) = changeURL
        getIssueID :: SearchNodesSearchResultItem -> Text
        getIssueID (SearchNodesIssue issueID _ _ _ _ _) = unpackID issueID
    getUpdatedAt :: SearchNodesSearchResultItem -> Either Text Timestamp
    getUpdatedAt (SearchNodesIssue _ _ (DateTime updatedAt') _ _ _) =
      case Timestamp.fromRFC3339 $ toLazy updatedAt' of
        Just ts -> Right ts
        Nothing -> Left $ "Unable to decode updatedAt format" <> show updatedAt'
    getLabels :: SearchNodesSearchResultItem -> [Either Text Text]
    getLabels issue =
      case issue of
        IssueLabels nodesLabel -> fmap getLabelFromNode nodesLabel
        respOther -> [Left ("Invalid response: " <> show respOther)]
    getLabelFromNode :: Maybe SearchNodesLabelsNodesLabel -> Either Text Text
    getLabelFromNode nodeLabelM = case nodeLabelM of
      Just
        (SearchNodesLabelsNodesLabel label) -> Right label
      Nothing -> Left "Missing Label in SearchNodesLabelsNodesLabel"
    getTDChangeUrls :: SearchNodesSearchResultItem -> [Either Text Text]
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
        respOther -> [Left ("Invalid response: " <> show respOther)]
    extractUrl :: Maybe SearchNodesTimelineItemsNodesIssueTimelineItems -> Either Text Text
    extractUrl item = case item of
      Just
        ( SearchNodesTimelineItemsNodesConnectedEvent
            "ConnectedEvent"
            (SearchNodesTimelineItemsNodesSubjectPullRequest (Just (URI url')))
          ) -> Right url'
      Just
        ( SearchNodesTimelineItemsNodesCrossReferencedEvent
            "CrossReferencedEvent"
            (SearchNodesTimelineItemsNodesSourcePullRequest (Just (URI url')))
          ) -> Right url'
      -- We are requesting Issue with connected PR we cannot get Nothing
      _ -> Left "Missing PR URI"
