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
import Data.Time.Format
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille (MonadGraphQLE)
import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GraphQL
import Monocle.Prelude
import Monocle.Search (TaskData (..))

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- The query muste use the filter "linked:pr" filter. steamLinkedIssue ensures this.
-- CONNECTED_EVENT happens when a LINK is performed manually though the GH UI.
-- CROSS_CONNECTED_EVENT happens when a PR reference an issue in some way
--  Most of the time this is used by developers to reference an issue in
--  the PR description or in a commit messages.
-- See GH documentation https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
defineByDocumentFile
  ghSchemaLocation
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
          __typename
          ... on Issue {
            id
            title
            updatedAt
            url
            number
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
                      __typename
                      ... on PullRequest {url}
                   }
                }
                ... on ConnectedEvent {
                   subject {
                     __typename
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

streamLinkedIssue :: MonadGraphQLE m => GraphClient -> UTCTime -> Text -> Stream (Of TaskData) m ()
streamLinkedIssue client time repo = streamFetch client mkArgs (Just getRateLimit) transformResponse
  where
    mkArgs =
      GetLinkedIssuesArgs
        ( from $ "repo:" <> from repo <> " updated:>=" <> toSimpleDate time <> " linked:pr"
        )
    toSimpleDate :: UTCTime -> String
    toSimpleDate utctime' = formatTime defaultTimeLocale "%F" utctime'

pattern IssueLabels nodesLabel <- SearchNodesIssue _ _ _ _ _ _ (Just (SearchNodesLabelsLabelConnection (Just nodesLabel))) _

transformResponse :: GetLinkedIssues -> (PageInfo, Maybe RateLimit, [Text], [TaskData])
transformResponse searchResult =
  case searchResult of
    GetLinkedIssues
      (Just (RateLimitRateLimit used remaining (DateTime resetAt')))
      ( SearchSearchResultItemConnection
          issueCount'
          (SearchPageInfoPageInfo hasNextPage' endCursor')
          (Just issues)
        ) ->
        let newTaskDataE = concatMap mkTaskData issues
            rateLimit = case parseDateValue $ from resetAt' of
              Just resetAt -> RateLimit {..}
              Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAt'
         in ( PageInfo hasNextPage' endCursor' (Just issueCount'),
              Just rateLimit,
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
        <*> pure (toLazy $ getNumber issue)
        <*> pure (toLazy $ getIssueURL issue)
        <*> pure (toLazy $ title issue)
        <*> pure "low"
        <*> pure "low"
        <*> pure 0
        <*> pure "gh#"
      where
        getLabelsE :: Either Text [LText]
        getLabelsE = case partitionEithers (getLabels issue) of
          ([], labels') -> Right (fmap toLazy labels')
          (errors, _) -> Left (unwords errors)
        getIssueURL :: SearchNodesSearchResultItem -> Text
        getIssueURL (SearchNodesIssue _ _ _ _ (URI changeURL) _ _ _) = changeURL
        getNumber :: SearchNodesSearchResultItem -> Text
        getNumber (SearchNodesIssue _ _ _ _ _ number _ _) = show number
    getUpdatedAt :: SearchNodesSearchResultItem -> Either Text Timestamp
    getUpdatedAt (SearchNodesIssue _ _ _ (DateTime updatedAt') _ _ _ _) =
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
          _
          _
          ( SearchNodesTimelineItemsIssueTimelineItemsConnection
              (Just urls)
            ) -> Right <$> mapMaybe extractUrl urls
        respOther -> [Left ("Invalid response: " <> show respOther)]
    extractUrl :: Maybe SearchNodesTimelineItemsNodesIssueTimelineItems -> Maybe Text
    extractUrl item = case item of
      Just
        ( SearchNodesTimelineItemsNodesConnectedEvent
            "ConnectedEvent"
            (SearchNodesTimelineItemsNodesSubjectPullRequest _ (Just (URI url')))
          ) -> Just url'
      Just
        ( SearchNodesTimelineItemsNodesCrossReferencedEvent
            "CrossReferencedEvent"
            (SearchNodesTimelineItemsNodesSourcePullRequest _ (Just (URI url')))
          ) -> Just url'
      -- We are requesting Issue with connected PR we cannot get Nothing
      _ -> Nothing
