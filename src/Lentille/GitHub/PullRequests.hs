{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- witch instance for Int32
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.PullRequests where

import Data.Morpheus.Client
import Data.Text qualified (takeWhile, takeWhileEnd)
import Google.Protobuf.Timestamp qualified as T
import Lentille
import Lentille.GitHub.RateLimit (getRateLimit, retryCheck)
import Lentille.GraphQL
import Monocle.Prelude hiding (id, state)
import Monocle.Protob.Change
import Proto3.Suite (Enumerated (Enumerated))
import Streaming.Prelude qualified as S (break)

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype GitObjectID = GitObjectID Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.github.com/en/graphql/reference/objects#pullrequest
defineByDocumentFile
  ghSchemaLocation
  [gql|
    query GetProjectPullRequests ($org: String!, $repo: String!, $depth: Int, $cursor: String) {
      rateLimit {
        used
        remaining
        resetAt
      }
      repository(owner: $org, name: $repo) {
        pullRequests (first: $depth, after: $cursor, orderBy: { field: UPDATED_AT, direction: DESC }) {
          totalCount
          pageInfo {hasNextPage endCursor}
          nodes {
            __typename
            ... on PullRequest {
              ...prdata
            }
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

streamPullRequests ::
  (HasLogger m, MonadGraphQLE m) =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  UTCTime ->
  Text ->
  LentilleStream m Changes
streamPullRequests client cb untilDate repoFullname =
  breakOnDate $ streamFetch client mkArgs optParams transformResponse'
 where
  org = Data.Text.takeWhile (/= '/') repoFullname
  repo = Data.Text.takeWhileEnd (/= '/') repoFullname
  mkArgs = GetProjectPullRequestsArgs org repo
  optParams =
    let fpRetryCheck = Just retryCheck
        fpDepth = Just defaultDepthCount
        fpGetRatelimit = Just getRateLimit
     in StreamFetchOptParams {..}
  transformResponse' = transformResponse getHost cb
  defaultDepthCount = 25
  getHost =
    let host' = host client
     in if host' == "api.github.com" then "github.com" else host'
  -- This transform the stream by adding a limit.
  -- We don't care about the rest so we replace it with ()
  breakOnDate = fmap (pure ()) . S.break (isChangeTooOld untilDate)

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
         in (PageInfo {..}, Just rateLimit, [], mapMaybe transPR (catMaybes projectPRs))
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing
      , Nothing
      , ["Unknown GetProjectPullRequests response: " <> show result]
      , []
      )
 where
  getIdent :: Text -> Ident
  getIdent = toIdent host identCB
  getGhostIdent = ghostIdent host
  repoOwnerName :: RepositoryPullRequestsNodesRepositoryRepository -> (Text, Text)
  repoOwnerName
    ( RepositoryPullRequestsNodesRepositoryRepository
        (RepositoryPullRequestsNodesRepositoryOwnerRepositoryOwner _ login)
        name
      ) = (login, name)
  repoFullname :: RepositoryPullRequestsNodesRepositoryRepository -> Text
  repoFullname r = let (owner, name) = repoOwnerName r in owner <> "/" <> name
  getURL (URI url) = url
  getSHA (GitObjectID sha) = sha
  commitCount :: RepositoryPullRequestsNodesCommitsPullRequestCommitConnection -> Int
  commitCount (RepositoryPullRequestsNodesCommitsPullRequestCommitConnection count _) = count
  toChangedFiles :: RepositoryPullRequestsNodesFilesPullRequestChangedFileConnection -> [ChangedFile]
  toChangedFiles (RepositoryPullRequestsNodesFilesPullRequestChangedFileConnection nodes) =
    toChangedFile <$> catMaybes (fromMaybe [] nodes)
   where
    toChangedFile :: RepositoryPullRequestsNodesFilesNodesPullRequestChangedFile -> ChangedFile
    toChangedFile RepositoryPullRequestsNodesFilesNodesPullRequestChangedFile {..} =
      ChangedFile (from additions) (from deletions) (from path)
  toChangeFilePath :: ChangedFile -> ChangedFilePath
  toChangeFilePath (ChangedFile _ _ path) = ChangedFilePath path
  toChangeCommits :: RepositoryPullRequestsNodesCommitsPullRequestCommitConnection -> [Commit]
  toChangeCommits (RepositoryPullRequestsNodesCommitsPullRequestCommitConnection _ nodes) =
    toCommit . commit <$> catMaybes (fromMaybe [] nodes)
   where
    toCommit :: RepositoryPullRequestsNodesCommitsNodesCommitCommit -> Commit
    toCommit RepositoryPullRequestsNodesCommitsNodesCommitCommit {..} =
      let commitSha = from $ getSHA oid
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
        ( RepositoryPullRequestsNodesCommitsNodesCommitAuthorGitActor
            (Just (RepositoryPullRequestsNodesCommitsNodesCommitAuthorUserUser login))
          ) = getIdent login
      getAuthor _ = ghostIdent host
      getCommitter
        ( RepositoryPullRequestsNodesCommitsNodesCommitCommitterGitActor
            (Just (RepositoryPullRequestsNodesCommitsNodesCommitCommitterUserUser login))
          ) = getIdent login
      getCommitter _ = ghostIdent host
  getPRAuthor :: RepositoryPullRequestsNodesAuthorActor -> Text
  getPRAuthor (RepositoryPullRequestsNodesAuthorActor _ login) = login
  getPRMergedBy :: RepositoryPullRequestsNodesMergedByActor -> Text
  getPRMergedBy (RepositoryPullRequestsNodesMergedByActor _ login) = login
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
  toLabels :: RepositoryPullRequestsNodesLabelsLabelConnection -> [Text]
  toLabels (RepositoryPullRequestsNodesLabelsLabelConnection nodes) =
    toLabel <$> catMaybes (fromMaybe [] nodes)
   where
    toLabel :: RepositoryPullRequestsNodesLabelsNodesLabel -> Text
    toLabel (RepositoryPullRequestsNodesLabelsNodesLabel name) = name
  toAssignees :: RepositoryPullRequestsNodesAssigneesUserConnection -> [Ident]
  toAssignees (RepositoryPullRequestsNodesAssigneesUserConnection nodes) =
    getIdent . toAssignee <$> catMaybes (fromMaybe [] nodes)
   where
    toAssignee :: RepositoryPullRequestsNodesAssigneesNodesUser -> Text
    toAssignee (RepositoryPullRequestsNodesAssigneesNodesUser login) = login
  toApprovals :: PullRequestReviewDecision -> Text
  toApprovals = \case
    PullRequestReviewDecisionAPPROVED -> "APPROVED"
    PullRequestReviewDecisionCHANGES_REQUESTED -> "CHANGES_REQUESTED"
    PullRequestReviewDecisionREVIEW_REQUIRED -> "REVIEW_REQUIRED"
  baseEvent :: ChangeEventType -> LText -> Change -> ChangeEvent
  baseEvent eType eId change =
    ChangeEvent
      { changeEventRepositoryPrefix = changeRepositoryPrefix change
      , changeEventRepositoryFullname = changeRepositoryFullname change
      , changeEventRepositoryShortname = changeRepositoryShortname change
      , changeEventBranch = changeBranch change
      , changeEventTargetBranch = changeTargetBranch change
      , changeEventNumber = changeNumber change
      , changeEventChangeId = changeChangeId change
      , changeEventUrl = changeUrl change
      , changeEventOnAuthor = changeAuthor change
      , changeEventOnCreatedAt = changeCreatedAt change
      , changeEventChangedFiles = toChangeFilePath <$> changeChangedFiles change
      , changeEventLabels = changeLabels change
      , changeEventId = eId
      , changeEventType = Just eType
      , -- To be filled by caller function
        changeEventCreatedAt = Nothing
      , changeEventAuthor = Nothing
      , changeEventOptionalDuration = swapDuration <$> changeOptionalDuration change
      }
  getEventsFromTimeline :: Change -> RepositoryPullRequestsNodesTimelineItemsPullRequestTimelineItemsConnection -> [ChangeEvent]
  getEventsFromTimeline change (RepositoryPullRequestsNodesTimelineItemsPullRequestTimelineItemsConnection nodes) =
    mapMaybe toEventM (catMaybes (fromMaybe [] nodes))
   where
    getID (ID v) = v
    toEventM :: RepositoryPullRequestsNodesTimelineItemsNodesPullRequestTimelineItems -> Maybe ChangeEvent
    toEventM = \case
      RepositoryPullRequestsNodesTimelineItemsNodesClosedEvent
        _
        eId
        createdAt
        (Just (RepositoryPullRequestsNodesTimelineItemsNodesActorActor _ actor)) ->
          if isMerged (changeState change) || isClosed (changeState change)
            then
              Just
                ( baseEvent
                    ( if isMerged $ changeState change
                        then ChangeEventTypeChangeMerged ChangeMergedEvent
                        else ChangeEventTypeChangeAbandoned ChangeAbandonedEvent
                    )
                    (from $ getID eId)
                    change
                )
                  { changeEventAuthor = Just $ getIdent actor
                  , changeEventCreatedAt = Just $ from createdAt
                  }
            else Nothing
      RepositoryPullRequestsNodesTimelineItemsNodesPullRequestReview
        _
        eId
        createdAt
        reviewState
        (Just (RepositoryPullRequestsNodesTimelineItemsNodesAuthorActor _ actor)) ->
          let approval = case reviewState of
                PullRequestReviewStateAPPROVED -> "APPROVED"
                PullRequestReviewStateCHANGES_REQUESTED -> "CHANGES_REQUESTED"
                PullRequestReviewStateCOMMENTED -> "COMMENTED"
                PullRequestReviewStateDISMISSED -> "DISMISSED"
                PullRequestReviewStatePENDING -> "PENDING"
              event =
                ( baseEvent
                    (ChangeEventTypeChangeReviewed $ ChangeReviewedEvent $ fromList [approval])
                    (from $ getID eId)
                    change
                )
                  { changeEventAuthor = Just $ getIdent actor
                  , changeEventCreatedAt = Just $ from createdAt
                  }
           in Just event
      RepositoryPullRequestsNodesTimelineItemsNodesHeadRefForcePushedEvent
        _
        eId
        createdAt
        (Just (RepositoryPullRequestsNodesTimelineItemsNodesFpactorActor _ actor)) ->
          Just
            ( baseEvent
                (ChangeEventTypeChangeCommitForcePushed ChangeCommitForcePushedEvent)
                (from $ getID eId)
                change
            )
              { changeEventAuthor = Just $ getIdent actor
              , changeEventCreatedAt = Just $ from createdAt
              }
      _ -> Nothing
  getCommitEvents :: Change -> RepositoryPullRequestsNodesCommitsPullRequestCommitConnection -> [ChangeEvent]
  getCommitEvents change (RepositoryPullRequestsNodesCommitsPullRequestCommitConnection _ nodes) =
    toEvent <$> catMaybes (fromMaybe [] nodes)
   where
    toEvent :: RepositoryPullRequestsNodesCommitsNodesPullRequestCommit -> ChangeEvent
    toEvent (RepositoryPullRequestsNodesCommitsNodesPullRequestCommit commit) =
      ( baseEvent
          (ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent)
          (from . getSHA $ oid commit)
          change
      )
        { changeEventAuthor = Just $ getCommitter $ committer commit
        , changeEventCreatedAt = maybe (changeCreatedAt change) (Just . from) (pushedDate commit)
        }
     where
      getCommitter :: Maybe RepositoryPullRequestsNodesCommitsNodesCommitCommitterGitActor -> Ident
      getCommitter
        ( Just
            ( RepositoryPullRequestsNodesCommitsNodesCommitCommitterGitActor
                (Just (RepositoryPullRequestsNodesCommitsNodesCommitCommitterUserUser login))
              )
          ) = getIdent login
      getCommitter _ = getGhostIdent
  getCommentEvents :: Change -> RepositoryPullRequestsNodesCommentsIssueCommentConnection -> [ChangeEvent]
  getCommentEvents change (RepositoryPullRequestsNodesCommentsIssueCommentConnection nodes) =
    mapMaybe toEvent (catMaybes (fromMaybe [] nodes))
   where
    toEvent :: RepositoryPullRequestsNodesCommentsNodesIssueComment -> Maybe ChangeEvent
    toEvent
      ( RepositoryPullRequestsNodesCommentsNodesIssueComment
          (ID eId)
          createdAt
          (Just (RepositoryPullRequestsNodesCommentsNodesAuthorActor _ actor))
        ) =
        Just
          ( baseEvent
              (ChangeEventTypeChangeCommented ChangeCommentedEvent)
              (from eId)
              change
          )
            { changeEventAuthor = Just $ getIdent actor
            , changeEventCreatedAt = Just $ from createdAt
            }
    toEvent _ = Nothing
  transPR :: RepositoryPullRequestsNodesPullRequest -> Maybe (Change, [ChangeEvent])
  transPR RepositoryPullRequestsNodesPullRequest {..} =
    let change =
          Change
            { changeId = from . sanitizeID $ unpackID id
            , changeNumber = from number
            , changeChangeId = getChangeId (repoFullname repository) (show number)
            , changeTitle = from title
            , changeText = from bodyText
            , changeUrl = from . getURL $ fromMaybe (error "Unable to decode change w/o webURL") webURL
            , changeCommitCount = from $ commitCount commits
            , changeAdditions = from additions
            , changeDeletions = from deletions
            , changeChangedFilesCount = from changedFiles
            , changeChangedFiles = fromList $ maybe mempty toChangedFiles files
            , changeCommits = fromList $ toChangeCommits commits
            , changeRepositoryPrefix = from . fst $ repoOwnerName repository
            , changeRepositoryFullname = from $ repoFullname repository
            , changeRepositoryShortname = from . snd $ repoOwnerName repository
            , changeAuthor = Just $ maybe getGhostIdent (getIdent . getPRAuthor) author
            , changeOptionalMergedBy = ChangeOptionalMergedByMergedBy . getIdent . getPRMergedBy <$> mergedBy
            , changeBranch = from headRefName
            , changeTargetBranch = from baseRefName
            , changeCreatedAt = Just $ from createdAt
            , changeOptionalMergedAt = from <$> mergedAt
            , changeUpdatedAt = Just $ from updatedAt
            , changeOptionalClosedAt = from <$> closedAt
            , changeState = toPRState state
            , changeOptionalDuration =
                if isMerged (toPRState state) || isClosed (toPRState state)
                  then Just $ toDuration (fromMaybe updatedAt closedAt) createdAt
                  else Nothing
            , changeMergeable = from $ toPRMergeableState mergeable
            , changeLabels = fromList $ from <$> maybe [] toLabels labels
            , changeAssignees = fromList $ toAssignees assignees
            , changeApprovals = fromList $ maybe [] ((: []) . from . toApprovals) reviewDecision
            , changeDraft = isDraft
            , changeOptionalSelfMerged =
                Just . ChangeOptionalSelfMergedSelfMerged $
                  (getPRAuthor <$> author) == (getPRMergedBy <$> mergedBy)
            }
        createdEvent =
          ( baseEvent
              (ChangeEventTypeChangeCreated ChangeCreatedEvent)
              ("CCE" <> changeId change)
              change
          )
            { changeEventCreatedAt = changeCreatedAt change
            , changeEventAuthor = changeAuthor change
            }
     in Just
          ( change
          , [createdEvent]
              <> getEventsFromTimeline change timelineItems
              <> getCommitEvents change commits
              <> getCommentEvents change comments
          )
