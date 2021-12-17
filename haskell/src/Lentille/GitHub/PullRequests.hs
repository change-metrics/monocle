{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- witch instance for Int32
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.PullRequests where

import Data.Morpheus.Client
-- import Lentille (MonadGraphQLE)
-- import Lentille.GitHub.RateLimit (getRateLimit)

import qualified Google.Protobuf.Timestamp as T
import Lentille.GitLab.Adapter (fromIntToInt32, getChangeId, ghostIdent, isMerged, sanitizeID, toIdent)
import Lentille.GraphQL
import Monocle.Api.Config (removeTrailingSlash)
import Monocle.Change
import Monocle.Prelude hiding (id, state)
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
        prs: pullRequests(first: 100, after: $cursor, orderBy: { field: UPDATED_AT, direction: DESC }) {
          totalCount
          pageInfo {hasNextPage endCursor}
          nodes {
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
|]

instance From Int Int32 where
  from = fromIntToInt32

dateTimeToUTCTime :: DateTime -> UTCTime
dateTimeToUTCTime dt =
  let dtText = show dt
   in fromMaybe
        ( error $ "Unable to parse date string: " <> from dtText
        )
        (parseDateValue dtText)

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

transformResponse ::
  -- hostname of the provider
  Text ->
  -- baseUrl of the provider
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  -- The response payload
  GetProjectPullRequests ->
  (PageInfo, Maybe RateLimit, [Text], [(Change, [ChangeEvent])])
transformResponse host baseUrl identCB result = do
  case result of
    GetProjectPullRequests
      (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
      ( Just
          ( RepositoryRepository
              ( RepositoryPrsPullRequestConnection
                  totalCount'
                  (RepositoryPrsPageInfoPageInfo hasNextPage endCursor)
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
    getIdent :: Text -> Ident
    getIdent = toIdent host identCB
    getGhostIdent = ghostIdent host
    repoOwnerName :: RepositoryPrsNodesRepositoryRepository -> (Text, Text)
    repoOwnerName
      ( RepositoryPrsNodesRepositoryRepository
          (RepositoryPrsNodesRepositoryOwnerRepositoryOwner _ login)
          name
        ) = (login, name)
    repoFullname :: RepositoryPrsNodesRepositoryRepository -> Text
    repoFullname r = let (owner, name) = repoOwnerName r in owner <> "/" <> name
    webUrl :: Text -> Text -> Text
    webUrl fullName number = removeTrailingSlash baseUrl <> "/" <> fullName <> "/" <> number
    commitCount :: RepositoryPrsNodesCommitsPullRequestCommitConnection -> Int
    commitCount (RepositoryPrsNodesCommitsPullRequestCommitConnection count _) = count
    toChangedFiles :: RepositoryPrsNodesFilesPullRequestChangedFileConnection -> [ChangedFile]
    toChangedFiles (RepositoryPrsNodesFilesPullRequestChangedFileConnection nodes) =
      toChangedFile <$> catMaybes (fromMaybe [] nodes)
      where
        toChangedFile :: RepositoryPrsNodesFilesNodesPullRequestChangedFile -> ChangedFile
        toChangedFile RepositoryPrsNodesFilesNodesPullRequestChangedFile {..} =
          ChangedFile (fromIntToInt32 additions) (fromIntToInt32 deletions) (from path)
    toChangeFilePath :: ChangedFile -> ChangedFilePath
    toChangeFilePath (ChangedFile _ _ path) = ChangedFilePath path
    toChangeCommits :: RepositoryPrsNodesCommitsPullRequestCommitConnection -> [Commit]
    toChangeCommits (RepositoryPrsNodesCommitsPullRequestCommitConnection _ nodes) =
      toCommit . commit <$> catMaybes (fromMaybe [] nodes)
      where
        toCommit :: RepositoryPrsNodesCommitsNodesCommitCommit -> Commit
        toCommit RepositoryPrsNodesCommitsNodesCommitCommit {..} =
          let commitSha = show oid
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
              ( RepositoryPrsNodesCommitsNodesCommitAuthorGitActor
                  (Just (RepositoryPrsNodesCommitsNodesCommitAuthorUserUser login))
                ) = getIdent login
            getAuthor _ = ghostIdent host
            getCommitter
              ( RepositoryPrsNodesCommitsNodesCommitCommitterGitActor
                  (Just (RepositoryPrsNodesCommitsNodesCommitCommitterUserUser login))
                ) = getIdent login
            getCommitter _ = ghostIdent host
    getPRAuthor :: RepositoryPrsNodesAuthorActor -> Text
    getPRAuthor (RepositoryPrsNodesAuthorActor _ login) = login
    getPRMergedBy :: RepositoryPrsNodesMergedByActor -> Text
    getPRMergedBy (RepositoryPrsNodesMergedByActor _ login) = login
    toPRState :: PullRequestState -> Enumerated Change_ChangeState
    toPRState = \case
      PullRequestStateCLOSED -> Enumerated (Right Change_ChangeStateClosed)
      PullRequestStateMERGED -> Enumerated $ Right Change_ChangeStateMerged
      PullRequestStateOPEN -> Enumerated $ Right Change_ChangeStateOpen
    toDuration :: DateTime -> DateTime -> ChangeOptionalDuration
    toDuration d1 d2 = ChangeOptionalDurationDuration . from $ diffUTCTimeToSec (from d1) (from d2)
    toPRMergeableState :: MergeableState -> Text
    toPRMergeableState = \case
      MergeableStateCONFLICTING -> "CONFLICT"
      MergeableStateMERGEABLE -> "MERGEABLE"
      MergeableStateUNKNOWN -> "MERGEABLE"
    toLabels :: RepositoryPrsNodesLabelsLabelConnection -> [Text]
    toLabels (RepositoryPrsNodesLabelsLabelConnection nodes) =
      toLabel <$> catMaybes (fromMaybe [] nodes)
      where
        toLabel :: RepositoryPrsNodesLabelsNodesLabel -> Text
        toLabel (RepositoryPrsNodesLabelsNodesLabel name) = name
    toAssignees :: RepositoryPrsNodesAssigneesUserConnection -> [Ident]
    toAssignees (RepositoryPrsNodesAssigneesUserConnection nodes) =
      getIdent . toAssignee <$> catMaybes (fromMaybe [] nodes)
      where
        toAssignee :: RepositoryPrsNodesAssigneesNodesUser -> Text
        toAssignee (RepositoryPrsNodesAssigneesNodesUser login) = login
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
    getEventsFromTimeline :: Change -> RepositoryPrsNodesTimelineItemsPullRequestTimelineItemsConnection -> [ChangeEvent]
    getEventsFromTimeline change (RepositoryPrsNodesTimelineItemsPullRequestTimelineItemsConnection nodes) =
      catMaybes $ toEventM <$> catMaybes (fromMaybe [] nodes)
      where
        toEventM :: RepositoryPrsNodesTimelineItemsNodesPullRequestTimelineItems -> Maybe ChangeEvent
        toEventM = \case
          RepositoryPrsNodesTimelineItemsNodesClosedEvent
            _
            _
            createdAt
            (Just (RepositoryPrsNodesTimelineItemsNodesActorActor _ actor)) ->
              Just
                ( baseEvent
                    (ChangeEventTypeChangeAbandoned ChangeAbandonedEvent)
                    ("ChangeAbandonedEvent-" <> changeId change)
                    change
                )
                  { changeEventAuthor = Just $ getIdent actor,
                    changeEventCreatedAt = Just $ from createdAt
                  }
          RepositoryPrsNodesTimelineItemsNodesPullRequestReview
            _
            _
            createdAt
            reviewState
            (Just (RepositoryPrsNodesTimelineItemsNodesAuthorActor _ actor)) ->
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
          RepositoryPrsNodesTimelineItemsNodesHeadRefForcePushedEvent
            _
            _
            createdAt
            (Just (RepositoryPrsNodesTimelineItemsNodesFpactorActor _ actor)) ->
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
    getCommitEvents :: Change -> RepositoryPrsNodesCommitsPullRequestCommitConnection -> [ChangeEvent]
    getCommitEvents change (RepositoryPrsNodesCommitsPullRequestCommitConnection _ nodes) =
      toEvent <$> catMaybes (fromMaybe [] nodes)
      where
        toEvent :: RepositoryPrsNodesCommitsNodesPullRequestCommit -> ChangeEvent
        toEvent (RepositoryPrsNodesCommitsNodesPullRequestCommit commit) =
          let actor = ""
           in ( baseEvent
                  (ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent)
                  ("ChangeCommitPushedEvent-" <> changeId change)
                  change
              )
                { changeEventAuthor = Just $ getIdent actor,
                  changeEventCreatedAt = maybe (changeCreatedAt change) (Just . from) (pushedDate commit)
                }
    getCommentEvents :: Change -> RepositoryPrsNodesCommentsIssueCommentConnection -> [ChangeEvent]
    getCommentEvents change (RepositoryPrsNodesCommentsIssueCommentConnection nodes) =
      catMaybes $ toEvent <$> catMaybes (fromMaybe [] nodes)
      where
        toEvent :: RepositoryPrsNodesCommentsNodesIssueComment -> Maybe ChangeEvent
        toEvent
          ( RepositoryPrsNodesCommentsNodesIssueComment
              (ID eId)
              createdAt
              (Just (RepositoryPrsNodesCommentsNodesAuthorActor _ actor))
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
    transPR :: RepositoryPrsNodesPullRequest -> (Change, [ChangeEvent])
    transPR RepositoryPrsNodesPullRequest {..} =
      let change =
            Change
              { changeId = from . sanitizeID $ unpackID id,
                changeNumber = from number,
                changeChangeId = getChangeId (repoFullname repository) (show number),
                changeTitle = from title,
                changeText = from bodyText,
                changeUrl = from $ webUrl (repoFullname repository) (show number),
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
       in ( change,
            [createdEvent]
              <> mergedEvent
              <> getEventsFromTimeline change timelineItems
              <> getCommitEvents change commits
              <> getCommentEvents change comments
          )
