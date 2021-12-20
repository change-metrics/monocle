{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- witch instance for Int32
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.PullRequests where

import Data.Morpheus.Client
-- import Lentille.GitHub.RateLimit (getRateLimit)

import Data.Time.Format
import qualified Google.Protobuf.Timestamp as T
import Lentille
import Lentille.GitHub.RateLimit (getRateLimit)
import Lentille.GraphQL
import Monocle.Change
import Monocle.Prelude hiding (id, state)
import Proto3.Suite (Enumerated (Enumerated))

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype GitObjectID = GitObjectID Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.github.com/en/graphql/reference/objects#pullrequest
defineByDocumentFile
  ghSchemaLocation
  [gql|
    query GetProjectPullRequests ($qs: String!, $cursor: String)  {
      rateLimit {
        used
        remaining
        resetAt
      }
      search(query: $qs, type: ISSUE, first: 30, after: $cursor) {
        issueCount
        pageInfo {hasNextPage endCursor}
        nodes {
          __typename
          ... on PullRequest {
            ...prdata
          }
        }
      }
    }
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

dateTimeToUTCTime :: DateTime -> UTCTime
dateTimeToUTCTime dt =
  let dtText = unDatetime dt
   in fromMaybe
        ( error $ "Unable to parse date string: " <> from dtText
        )
        (parseDateValue dtText)
  where
    unDatetime :: DateTime -> String
    unDatetime (DateTime s) = from s

dateTimeToTimestamp :: DateTime -> T.Timestamp
dateTimeToTimestamp = T.fromUTCTime . dateTimeToUTCTime

instance From DateTime UTCTime where
  from = dateTimeToUTCTime

instance From DateTime T.Timestamp where
  from = dateTimeToTimestamp

instance From DateTime ChangeOptionalMergedAt where
  from = ChangeOptionalMergedAtMergedAt . dateTimeToTimestamp

instance From DateTime ChangeOptionalClosedAt where
  from = ChangeOptionalClosedAtClosedAt . dateTimeToTimestamp

type Changes = (Change, [ChangeEvent])

data GHQueryArgs = GHQueryArgs
  { repo :: Text,
    time :: Maybe UTCTime,
    extraQ :: Maybe Text
  }

getPullRequestStream ::
  MonadGraphQLE m =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  UTCTime ->
  Text ->
  LentilleStream m Changes
getPullRequestStream client cb time repo =
  let args = GHQueryArgs repo (Just time) Nothing
   in streamPullRequests client cb args

getQS :: GHQueryArgs -> Text
getQS GHQueryArgs {..} = unwords ["is:pr", toRepoFrag, toDateFrag, toExtraQ]
  where
    toRepoFrag = from $ "repo:" <> repo
    toDateFrag = from $ maybe "" (\d -> "updated:>=" <> formatTime defaultTimeLocale "%F" d) time
    toExtraQ = from (fromMaybe mempty extraQ)

streamPullRequests ::
  MonadGraphQLE m =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  GHQueryArgs ->
  LentilleStream m Changes
streamPullRequests client cb qArgs = streamFetch client mkArgs (Just getRateLimit) transformResponse'
  where
    mkArgs =
      GetProjectPullRequestsArgs $ getQS qArgs
    transformResponse' = transformResponse getHost cb
    getHost =
      let host' = host client
       in if host' == "api.github.com" then "github.com" else host'

transformResponse ::
  -- hostname of the provider
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  -- The response payload
  GetProjectPullRequests ->
  (PageInfo, Maybe RateLimit, [Text], [(Change, [ChangeEvent])])
transformResponse host identCB result = do
  case result of
    GetProjectPullRequests
      (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
      ( SearchSearchResultItemConnection
          totalCount'
          (SearchPageInfoPageInfo hasNextPage endCursor)
          (Just projectPRs)
        ) ->
        let rateLimit = case parseDateValue $ from resetAtText of
              Just resetAt -> RateLimit {..}
              Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
            totalCount = Just totalCount'
         in (PageInfo {..}, Just rateLimit, [], catMaybes $ transPR <$> catMaybes projectPRs)
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing,
        Nothing,
        ["Unknown GetProjectPullRequests response: " <> show result],
        []
      )
  where
    getIdent :: Text -> Ident
    getIdent = toIdent host identCB
    getGhostIdent = ghostIdent host
    repoOwnerName :: SearchNodesRepositoryRepository -> (Text, Text)
    repoOwnerName
      ( SearchNodesRepositoryRepository
          (SearchNodesRepositoryOwnerRepositoryOwner _ login)
          name
        ) = (login, name)
    repoFullname :: SearchNodesRepositoryRepository -> Text
    repoFullname r = let (owner, name) = repoOwnerName r in owner <> "/" <> name
    getURL :: URI -> Text
    getURL (URI url) = url
    commitCount :: SearchNodesCommitsPullRequestCommitConnection -> Int
    commitCount (SearchNodesCommitsPullRequestCommitConnection count _) = count
    toChangedFiles :: SearchNodesFilesPullRequestChangedFileConnection -> [ChangedFile]
    toChangedFiles (SearchNodesFilesPullRequestChangedFileConnection nodes) =
      toChangedFile <$> catMaybes (fromMaybe [] nodes)
      where
        toChangedFile :: SearchNodesFilesNodesPullRequestChangedFile -> ChangedFile
        toChangedFile SearchNodesFilesNodesPullRequestChangedFile {..} =
          ChangedFile (from additions) (from deletions) (from path)
    toChangeFilePath :: ChangedFile -> ChangedFilePath
    toChangeFilePath (ChangedFile _ _ path) = ChangedFilePath path
    toChangeCommits :: SearchNodesCommitsPullRequestCommitConnection -> [Commit]
    toChangeCommits (SearchNodesCommitsPullRequestCommitConnection _ nodes) =
      toCommit . commit <$> catMaybes (fromMaybe [] nodes)
      where
        toCommit :: SearchNodesCommitsNodesCommitCommit -> Commit
        toCommit SearchNodesCommitsNodesCommitCommit {..} =
          let commitSha = show $ getSHA oid
              commitAuthor = getAuthor <$> author
              commitCommitter = getCommitter <$> committer
              commitAuthoredAt = Just $ from authoredDate
              commitCommittedAt = Just $ from committedDate
              commitAdditions = from additions
              commitDeletions = from deletions
              commitTitle = from message
           in Commit {..}
          where
            getAuthor
              ( SearchNodesCommitsNodesCommitAuthorGitActor
                  (Just (SearchNodesCommitsNodesCommitAuthorUserUser login))
                ) = getIdent login
            getAuthor _ = ghostIdent host
            getCommitter
              ( SearchNodesCommitsNodesCommitCommitterGitActor
                  (Just (SearchNodesCommitsNodesCommitCommitterUserUser login))
                ) = getIdent login
            getCommitter _ = ghostIdent host
            getSHA (GitObjectID sha) = sha
    getPRAuthor :: SearchNodesAuthorActor -> Text
    getPRAuthor (SearchNodesAuthorActor _ login) = login
    getPRMergedBy :: SearchNodesMergedByActor -> Text
    getPRMergedBy (SearchNodesMergedByActor _ login) = login
    toPRState :: PullRequestState -> Enumerated Change_ChangeState
    toPRState = \case
      PullRequestStateCLOSED -> Enumerated (Right Change_ChangeStateClosed)
      PullRequestStateMERGED -> Enumerated $ Right Change_ChangeStateMerged
      PullRequestStateOPEN -> Enumerated $ Right Change_ChangeStateOpen
    toDuration :: DateTime -> DateTime -> ChangeOptionalDuration
    toDuration d1 d2 = ChangeOptionalDurationDuration . from $ diffTimeSec (from d1) (from d2)
    toPRMergeableState :: MergeableState -> Text
    toPRMergeableState = \case
      MergeableStateCONFLICTING -> "CONFLICT"
      MergeableStateMERGEABLE -> "MERGEABLE"
      MergeableStateUNKNOWN -> "UNKNOWN"
    toLabels :: SearchNodesLabelsLabelConnection -> [Text]
    toLabels (SearchNodesLabelsLabelConnection nodes) =
      toLabel <$> catMaybes (fromMaybe [] nodes)
      where
        toLabel :: SearchNodesLabelsNodesLabel -> Text
        toLabel (SearchNodesLabelsNodesLabel name) = name
    toAssignees :: SearchNodesAssigneesUserConnection -> [Ident]
    toAssignees (SearchNodesAssigneesUserConnection nodes) =
      getIdent . toAssignee <$> catMaybes (fromMaybe [] nodes)
      where
        toAssignee :: SearchNodesAssigneesNodesUser -> Text
        toAssignee (SearchNodesAssigneesNodesUser login) = login
    toApprovals :: PullRequestReviewDecision -> Text
    toApprovals = \case
      PullRequestReviewDecisionAPPROVED -> "APPROVED"
      PullRequestReviewDecisionCHANGES_REQUESTED -> "CHANGES_REQUESTED"
      PullRequestReviewDecisionREVIEW_REQUIRED -> "REVIEW_REQUIRED"
    baseEvent :: ChangeEventType -> LText -> Change -> ChangeEvent
    baseEvent eType eId change =
      ChangeEvent
        { changeEventRepositoryPrefix = changeRepositoryPrefix change,
          changeEventRepositoryFullname = changeRepositoryFullname change,
          changeEventRepositoryShortname = changeRepositoryShortname change,
          changeEventBranch = changeBranch change,
          changeEventTargetBranch = changeTargetBranch change,
          changeEventNumber = changeNumber change,
          changeEventChangeId = changeId change,
          changeEventUrl = changeUrl change,
          changeEventOnAuthor = changeAuthor change,
          changeEventOnCreatedAt = changeCreatedAt change,
          changeEventChangedFiles = toChangeFilePath <$> changeChangedFiles change,
          changeEventLabels = changeLabels change,
          changeEventId = eId,
          changeEventType = Just eType,
          -- To be filled by caller function
          changeEventCreatedAt = Nothing,
          changeEventAuthor = Nothing
        }
    getEventsFromTimeline :: Change -> SearchNodesTimelineItemsPullRequestTimelineItemsConnection -> [ChangeEvent]
    getEventsFromTimeline change (SearchNodesTimelineItemsPullRequestTimelineItemsConnection nodes) =
      catMaybes $ toEventM <$> catMaybes (fromMaybe [] nodes)
      where
        toEventM :: SearchNodesTimelineItemsNodesPullRequestTimelineItems -> Maybe ChangeEvent
        toEventM = \case
          SearchNodesTimelineItemsNodesClosedEvent
            _
            _
            createdAt
            (Just (SearchNodesTimelineItemsNodesActorActor _ actor)) ->
              if not . isMerged $ changeState change
                then
                  Just
                    ( baseEvent
                        (ChangeEventTypeChangeAbandoned ChangeAbandonedEvent)
                        ("ChangeAbandonedEvent-" <> changeId change)
                        change
                    )
                      { changeEventAuthor = Just $ getIdent actor,
                        changeEventCreatedAt = Just $ from createdAt
                      }
                else Nothing
          SearchNodesTimelineItemsNodesPullRequestReview
            _
            _
            createdAt
            reviewState
            (Just (SearchNodesTimelineItemsNodesAuthorActor _ actor)) ->
              let approval = case reviewState of
                    PullRequestReviewStateAPPROVED -> "APPROVED"
                    PullRequestReviewStateCHANGES_REQUESTED -> "CHANGES_REQUESTED"
                    PullRequestReviewStateCOMMENTED -> "COMMENTED"
                    PullRequestReviewStateDISMISSED -> "DISMISSED"
                    PullRequestReviewStatePENDING -> "PENDING"
                  event =
                    ( baseEvent
                        (ChangeEventTypeChangeReviewed $ ChangeReviewedEvent $ fromList [approval])
                        ("ChangeReviewedEvent-" <> changeId change)
                        change
                    )
                      { changeEventAuthor = Just $ getIdent actor,
                        changeEventCreatedAt = Just $ from createdAt
                      }
               in Just event
          SearchNodesTimelineItemsNodesHeadRefForcePushedEvent
            _
            _
            createdAt
            (Just (SearchNodesTimelineItemsNodesFpactorActor _ actor)) ->
              Just
                ( baseEvent
                    (ChangeEventTypeChangeCommitForcePushed ChangeCommitForcePushedEvent)
                    ("ChangeCommitForcePushedEvent-" <> changeId change)
                    change
                )
                  { changeEventAuthor = Just $ getIdent actor,
                    changeEventCreatedAt = Just $ from createdAt
                  }
          _ -> Nothing
    getCommitEvents :: Change -> SearchNodesCommitsPullRequestCommitConnection -> [ChangeEvent]
    getCommitEvents change (SearchNodesCommitsPullRequestCommitConnection _ nodes) =
      toEvent <$> catMaybes (fromMaybe [] nodes)
      where
        toEvent :: SearchNodesCommitsNodesPullRequestCommit -> ChangeEvent
        toEvent (SearchNodesCommitsNodesPullRequestCommit commit) =
          ( baseEvent
              (ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent)
              ("ChangeCommitPushedEvent-" <> changeId change)
              change
          )
            { changeEventAuthor = Just $ getCommitter $ committer commit,
              changeEventCreatedAt = maybe (changeCreatedAt change) (Just . from) (pushedDate commit)
            }
          where
            getCommitter :: Maybe SearchNodesCommitsNodesCommitCommitterGitActor -> Ident
            getCommitter
              ( Just
                  ( SearchNodesCommitsNodesCommitCommitterGitActor
                      (Just (SearchNodesCommitsNodesCommitCommitterUserUser login))
                    )
                ) = getIdent login
            getCommitter _ = getGhostIdent
    getCommentEvents :: Change -> SearchNodesCommentsIssueCommentConnection -> [ChangeEvent]
    getCommentEvents change (SearchNodesCommentsIssueCommentConnection nodes) =
      catMaybes $ toEvent <$> catMaybes (fromMaybe [] nodes)
      where
        toEvent :: SearchNodesCommentsNodesIssueComment -> Maybe ChangeEvent
        toEvent
          ( SearchNodesCommentsNodesIssueComment
              (ID eId)
              createdAt
              (Just (SearchNodesCommentsNodesAuthorActor _ actor))
            ) =
            Just
              ( baseEvent
                  (ChangeEventTypeChangeCommented ChangeCommentedEvent)
                  ("ChangeCommmentedEvent-" <> changeId change <> "-" <> from eId)
                  change
              )
                { changeEventAuthor = Just $ getIdent actor,
                  changeEventCreatedAt = Just $ from createdAt
                }
        toEvent _ = Nothing
    transPR :: SearchNodesSearchResultItem -> Maybe (Change, [ChangeEvent])
    transPR (SearchNodesSearchResultItem _) = Nothing
    transPR SearchNodesPullRequest {..} =
      let change =
            Change
              { changeId = from . sanitizeID $ unpackID id,
                changeNumber = from number,
                changeChangeId = getChangeId (repoFullname repository) (show number),
                changeTitle = from title,
                changeText = from bodyText,
                changeUrl = from . getURL $ fromMaybe (error "Unable to decode change w/o webURL") webURL,
                changeCommitCount = from $ commitCount commits,
                changeAdditions = from additions,
                changeDeletions = from deletions,
                changeChangedFilesCount = from changedFiles,
                changeChangedFiles = fromList $ maybe mempty toChangedFiles files,
                changeCommits = fromList $ toChangeCommits commits,
                changeRepositoryPrefix = from . fst $ repoOwnerName repository,
                changeRepositoryFullname = from $ repoFullname repository,
                changeRepositoryShortname = from . snd $ repoOwnerName repository,
                changeAuthor = Just $ maybe getGhostIdent (getIdent . getPRAuthor) author,
                changeOptionalMergedBy = ChangeOptionalMergedByMergedBy . getIdent . getPRMergedBy <$> mergedBy,
                changeBranch = from headRefName,
                changeTargetBranch = from baseRefName,
                changeCreatedAt = Just $ from createdAt,
                changeOptionalMergedAt = from <$> mergedAt,
                changeUpdatedAt = Just $ from updatedAt,
                changeOptionalClosedAt = from <$> closedAt,
                changeState = toPRState state,
                changeOptionalDuration = Just $ toDuration (fromMaybe updatedAt closedAt) createdAt,
                changeMergeable = from $ toPRMergeableState mergeable,
                changeLabels = fromList $ from <$> maybe [] toLabels labels,
                changeAssignees = fromList $ toAssignees assignees,
                changeApprovals = fromList $ maybe [] ((: []) . from . toApprovals) reviewDecision,
                changeDraft = isDraft,
                changeOptionalSelfMerged =
                  Just . ChangeOptionalSelfMergedSelfMerged $
                    (getPRAuthor <$> author) == (getPRMergedBy <$> mergedBy)
              }
          createdEvent =
            ( baseEvent
                (ChangeEventTypeChangeCreated ChangeCreatedEvent)
                ("ChangeCreatedEvent-" <> changeId change)
                change
            )
              { changeEventCreatedAt = changeCreatedAt change,
                changeEventAuthor = changeAuthor change
              }
          mergedEvent =
            [ ( baseEvent
                  (ChangeEventTypeChangeMerged ChangeMergedEvent)
                  ("ChangeMergedEvent-" <> changeId change)
                  change
              )
                { changeEventAuthor = getMergedByIdent <$> changeOptionalMergedBy change,
                  changeEventCreatedAt = getMergedAt <$> changeOptionalMergedAt change
                }
              | isMerged $ changeState change
            ]
            where
              getMergedByIdent (ChangeOptionalMergedByMergedBy ident) = ident
              getMergedAt (ChangeOptionalMergedAtMergedAt ts) = ts
       in Just
            ( change,
              [createdEvent]
                <> mergedEvent
                <> getEventsFromTimeline change timelineItems
                <> getCommitEvents change commits
                <> getCommentEvents change comments
            )
