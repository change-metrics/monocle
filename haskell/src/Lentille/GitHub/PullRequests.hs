{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.PullRequests where

import Data.Morpheus.Client
-- import Lentille (MonadGraphQLE)
-- import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GraphQL
import Monocle.Change
import Monocle.Prelude
import Proto3.Suite (Enumerated (Enumerated))

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype GitObjectID = GitObjectID Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.github.com/en/graphql/reference/objects#pullrequest
defineByDocumentFile
  ghSchemaLocation
  [gql|
    query GetProjectPullRequests ($owner: String!, $reponame: String!, $cursor: String)  {
      rateLimit {
        used
        remaining
        resetAt
      }
      repository(owner: $owner, name: $reponame) {
        pullRequests(first: 100, after: $cursor, orderBy: { field: UPDATED_AT, direction: DESC }) {
          totalCount
          pageInfo {hasNextPage endCursor}
          nodes {
            id
            updatedAt
            createdAt
            mergedAt
            closedAt
            author {login}
            mergedBy {login}
            repository {
              owner {login}
              name
            }
            additions
            deletions
            changedFiles
            title
            headRefName
            baseRefName
            bodyText
            state
            reviewDecision
            number
            mergeable
            isDraft
            labels (first: 100) {
              nodes {name}
            }
            assignees (first: 100) {
              nodes {login}
            }
            comments (first: 100) {
              nodes {
                id
                createdAt
                author {login}
              }
            }
            commits (first: 100) {
              totalCount
              nodes {
                commit {
                  oid
                  pushedDate
                  authoredDate
                  committedDate
                  additions
                  deletions
                  message
                  author {user {login}}
                  committer {user {login}}
                }
              }
            }
            files (first: 100) {
              nodes {
                additions
                deletions
                path
              }
            }
            timelineItems (first: 100 itemTypes: [
                CLOSED_EVENT, PULL_REQUEST_REVIEW, HEAD_REF_FORCE_PUSHED_EVENT
              ]) {
              nodes {
                __typename
                ... on ClosedEvent {
                  id
                  createdAt
                  actor {login}
                }
                ... on PullRequestReview {
                  id
                  createdAt
                  state
                  author {login}
                }
                ... on HeadRefForcePushedEvent {
                  id
                  createdAt
                  fpactor: actor {login}
                }
              }
            }
          }
        }
      }
    }
  |]

fakeChange :: Change
fakeChange =
  Change
    { changeId = "",
      changeNumber = 1,
      changeChangeId = "",
      changeTitle = "",
      changeText = "",
      changeUrl = "",
      changeCommitCount = 1,
      changeAdditions = 1,
      changeDeletions = 1,
      changeChangedFilesCount = 1,
      changeChangedFiles = mempty,
      changeCommits = mempty,
      changeRepositoryPrefix = "",
      changeRepositoryFullname = "",
      changeRepositoryShortname = "",
      changeAuthor = Nothing,
      changeOptionalMergedBy = Nothing,
      changeBranch = "",
      changeTargetBranch = "",
      changeCreatedAt = Nothing,
      changeOptionalMergedAt = Nothing,
      changeUpdatedAt = Nothing,
      changeOptionalClosedAt = Nothing,
      changeState = Enumerated $ Left 1,
      changeOptionalDuration = Nothing,
      changeMergeable = "",
      changeLabels = mempty,
      changeAssignees = mempty,
      changeApprovals = mempty,
      changeDraft = False,
      changeOptionalSelfMerged = Nothing
    }

fakeEvent :: ChangeEvent
fakeEvent =
  ChangeEvent
    { changeEventId = "",
      changeEventCreatedAt = Nothing,
      changeEventAuthor = Nothing,
      changeEventRepositoryPrefix = "",
      changeEventRepositoryFullname = "",
      changeEventRepositoryShortname = "",
      changeEventBranch = "",
      changeEventTargetBranch = "",
      changeEventNumber = 1,
      changeEventChangeId = "",
      changeEventUrl = "",
      changeEventOnAuthor = Nothing,
      changeEventOnCreatedAt = Nothing,
      changeEventChangedFiles = mempty,
      changeEventType = Nothing,
      changeEventLabels = mempty
    }

fakePageInfo :: PageInfo
fakePageInfo = PageInfo False Nothing Nothing

transformResponse :: GetProjectPullRequests -> (PageInfo, Maybe RateLimit, [Text], [(Change, [ChangeEvent])])
transformResponse result = do
  case result of
    GetProjectPullRequests
      (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
      ( Just
          ( RepositoryRepository
              ( RepositoryPullRequestsPullRequestConnection
                  totalCount'
                  (RepositoryPullRequestsPageInfoPageInfo hasNextPage endCursor)
                  (Just projectPRs)
                )
            )
        ) ->
        let rateLimit = case parseDateValue $ from resetAtText of
              Just resetAt -> RateLimit {..}
              Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
            totalCount = Just totalCount'
         in (PageInfo {..}, Just rateLimit, [], transPR <$> catMaybes projectPRs)
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing,
        Nothing,
        ["Unknown GetProjectPullRequests response: " <> show result],
        []
      )
  where
    transPR :: RepositoryPullRequestsNodesPullRequest -> (Change, [ChangeEvent])
    transPR _ = (fakeChange, [fakeEvent])
