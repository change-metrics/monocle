{-# LANGUAGE QuasiQuotes #-}

module Lentille.GitHub.GraphQLFragments (fragmentPRData) where

import Data.Morpheus.Client (raw)
import Data.Text

-- https://docs.github.com/en/graphql/reference/objects#pullrequest
fragmentPRData :: Text
fragmentPRData =
  [raw|
  fragment prdata on PullRequest {
    id
    updatedAt
    createdAt
    mergedAt
    closedAt
    author {
      __typename
      login
    }
    mergedBy {
      __typename
      login
    }
    repository {
      owner {
        __typename
        login
      }
      name
    }
    additions
    deletions
    changedFiles
    title
    webURL: url
    headRefName
    baseRefName
    bodyText
    state
    reviewDecision
    mergeCommit {
      __typename
      mergedCommitOid: oid
    }
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
        author {
          __typename
          login
        }
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
          actor {
            __typename
            login
          }
        }
        ... on PullRequestReview {
          id
          createdAt
          state
          author {
            __typename
            login
          }
        }
        ... on HeadRefForcePushedEvent {
          id
          createdAt
          fpactor: actor {
            __typename
            login
          }
        }
      }
    }
  }
|]
