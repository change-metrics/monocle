{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}

module Lentille.GitHub.Issues where

import Data.Morpheus.Client
import Data.Time.Clock
import Data.Time.Format
import Data.Vector qualified as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GitHub.Types
import Lentille.GraphQL
import Monocle.Prelude
import Monocle.Protob.Search (TaskData (..))

-- The query muste use the filter "linked:pr" filter. steamLinkedIssue ensures this.
-- CONNECTED_EVENT happens when a LINK is performed manually though the GH UI.
-- CROSS_CONNECTED_EVENT happens when a PR reference an issue in some way
--  Most of the time this is used by developers to reference an issue in
--  the PR description or in a commit messages.
-- See GH documentation https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
declareLocalTypesInline
  ghSchemaLocation
  [raw|
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

streamLinkedIssue ::
  GraphEffects es =>
  GraphClient ->
  UTCTime ->
  Text ->
  LentilleStream es TaskData
streamLinkedIssue client time repo =
  streamFetch client mkArgs optParams transformResponse
 where
  mkArgs _ =
    GetLinkedIssuesArgs
      ( from $ "repo:" <> from repo <> " updated:>=" <> toSimpleDate time <> " linked:pr"
      )
  optParams = defaultStreamFetchOptParams {fpGetRatelimit = Just getRateLimit}
  toSimpleDate :: UTCTime -> String
  toSimpleDate = formatTime defaultTimeLocale "%F"

pattern IssueLabels :: [Maybe GetLinkedIssuesSearchNodesLabelsNodes] -> GetLinkedIssuesSearchNodesIssue
pattern IssueLabels nodesLabel <- GetLinkedIssuesSearchNodesIssue _ _ _ _ _ _ (Just (GetLinkedIssuesSearchNodesLabels (Just nodesLabel))) _

transformRateLimit :: GetLinkedIssuesRateLimit -> RateLimit
transformRateLimit (GetLinkedIssuesRateLimit used remaining (DateTime resetAtText)) =
  case parseDateValue $ from resetAtText of
    Just resetAt -> RateLimit {..}
    Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText

transformResponse :: GetLinkedIssues -> (PageInfo, Maybe RateLimit, [Text], [TaskData])
transformResponse searchResult =
  case searchResult of
    GetLinkedIssues
      rateLimitM
      ( GetLinkedIssuesSearch
          issueCount'
          (GetLinkedIssuesSearchPageInfo hasNextPage' endCursor')
          (Just issues)
        ) ->
        let newTaskDataE = concatMap mkTaskData issues
         in ( PageInfo hasNextPage' endCursor' (Just issueCount')
            , transformRateLimit <$> rateLimitM
            , lefts newTaskDataE
            , rights newTaskDataE
            )
    respOther -> error ("Invalid response: " <> show respOther)
 where
  mkTaskData :: Maybe GetLinkedIssuesSearchNodes -> [Either Text TaskData]
  mkTaskData issueM = case issueM of
    Just issue ->
      let tdChangeUrlsE :: [Either Text Text]
          tdChangeUrlsE = getTDChangeUrls issue
          newTaskDataEE :: [Either Text (Either Text TaskData)]
          newTaskDataEE = (fmap . fmap $ toTaskData issue) tdChangeUrlsE
          newTaskDataE = fmap join newTaskDataEE
       in newTaskDataE
    Nothing -> []

  toTaskData :: GetLinkedIssuesSearchNodes -> Text -> Either Text TaskData
  toTaskData (GetLinkedIssuesSearchNodesVariantSearchResultItem _) _ = Left "Impossible result"
  toTaskData (GetLinkedIssuesSearchNodesVariantIssue issue) curl =
    TaskData
      <$> (Just <$> getUpdatedAt issue)
      <*> pure (from curl)
      <*> (V.fromList <$> getLabelsE)
      <*> pure (from $ getNumber issue)
      <*> pure (from $ getIssueURL issue)
      <*> pure (from $ title issue)
      <*> pure "low"
      <*> pure "low"
      <*> pure 0
      <*> pure "gh#"
   where
    getLabelsE :: Either Text [LText]
    getLabelsE = case partitionEithers (getLabels issue) of
      ([], labels') -> Right (fmap from labels')
      (errors, _) -> Left (unwords errors)
    getIssueURL :: GetLinkedIssuesSearchNodesIssue -> Text
    getIssueURL (GetLinkedIssuesSearchNodesIssue _ _ _ _ (URI changeURL) _ _ _) = changeURL
    getNumber :: GetLinkedIssuesSearchNodesIssue -> Text
    getNumber (GetLinkedIssuesSearchNodesIssue _ _ _ _ _ number _ _) = show number

  getUpdatedAt :: GetLinkedIssuesSearchNodesIssue -> Either Text Timestamp
  getUpdatedAt (GetLinkedIssuesSearchNodesIssue _ _ _ (DateTime updatedAt') _ _ _ _) =
    case Timestamp.fromRFC3339 $ from updatedAt' of
      Just ts -> Right ts
      Nothing -> Left $ "Unable to decode updatedAt format" <> show updatedAt'

  getLabels :: GetLinkedIssuesSearchNodesIssue -> [Either Text Text]
  getLabels issue =
    case issue of
      IssueLabels nodesLabel -> fmap getLabelFromNode nodesLabel
      respOther -> [Left ("Invalid response: " <> show respOther)]

  getLabelFromNode :: Maybe GetLinkedIssuesSearchNodesLabelsNodes -> Either Text Text
  getLabelFromNode nodeLabelM = case nodeLabelM of
    Just (GetLinkedIssuesSearchNodesLabelsNodes name) -> Right name
    Nothing -> Left "Missing Label in SearchNodesLabelsNodesLabel"

  getTDChangeUrls :: GetLinkedIssuesSearchNodes -> [Either Text Text]
  getTDChangeUrls issue =
    case issue of
      GetLinkedIssuesSearchNodesVariantIssue
        ( GetLinkedIssuesSearchNodesIssue
            _
            _
            _
            _
            _
            _
            _
            ( GetLinkedIssuesSearchNodesTimelineItems
                (Just urls)
              )
          ) -> Right <$> mapMaybe extractUrl urls
      respOther -> [Left ("Invalid response: " <> show respOther)]

  extractUrl :: Maybe GetLinkedIssuesSearchNodesTimelineItemsNodes -> Maybe Text
  extractUrl item = case item of
    Just
      ( GetLinkedIssuesSearchNodesTimelineItemsNodesVariantConnectedEvent
          ( GetLinkedIssuesSearchNodesTimelineItemsNodesConnectedEvent
              "ConnectedEvent"
              ( GetLinkedIssuesSearchNodesTimelineItemsNodesSubjectVariantPullRequest
                  (GetLinkedIssuesSearchNodesTimelineItemsNodesSubjectPullRequest _ (URI url'))
                )
            )
        ) -> Just url'
    Just
      ( GetLinkedIssuesSearchNodesTimelineItemsNodesVariantCrossReferencedEvent
          ( GetLinkedIssuesSearchNodesTimelineItemsNodesCrossReferencedEvent
              "CrossReferencedEvent"
              ( GetLinkedIssuesSearchNodesTimelineItemsNodesSourceVariantPullRequest
                  (GetLinkedIssuesSearchNodesTimelineItemsNodesSourcePullRequest _ (URI url'))
                )
            )
        ) -> Just url'
    -- We are requesting Issue with connected PR we cannot get Nothing
    _ -> Nothing
