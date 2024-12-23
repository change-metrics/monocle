{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.PullRequests where

import Data.Morpheus.Client
import Data.Text qualified (takeWhile, takeWhileEnd)
import Lentille
import Lentille.GitHub.GraphQLFragments (fragmentPRData)
import Lentille.GitHub.Types
import Lentille.GitHub.Utils
import Lentille.GraphQL
import Monocle.Config qualified as Config
import Monocle.Prelude hiding (id, state)
import Monocle.Protob.Change

-- https://docs.github.com/en/graphql/reference/queries#repository
declareLocalTypesInline
  ghSchemaLocation
  ( [raw|
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
|]
      <> fragmentPRData
  )

streamPullRequests ::
  GraphEffects es =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Config.IdentUG) ->
  UTCTime ->
  Text ->
  LentilleStream es Changes
streamPullRequests client cb untilDate repoFullname =
  streamDropBefore untilDate $ streamFetch client mkArgs optParams transformResponse'
 where
  org = Data.Text.takeWhile (/= '/') repoFullname
  repo = Data.Text.takeWhileEnd (/= '/') repoFullname
  mkArgs = GetProjectPullRequestsArgs org repo
  transformResponse' = transformResponse (getHost client) cb

transformResponse ::
  -- hostname of the provider
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Config.IdentUG) ->
  -- The response payload
  GetProjectPullRequests ->
  (PageInfo, Maybe RateLimit, GraphResponseResult, [Changes])
transformResponse host identCB result = do
  let process resp rateLimit = case resp of
        ( Just
            ( GetProjectPullRequestsRepository
                ( GetProjectPullRequestsRepositoryPullRequests
                    totalCount'
                    (GetProjectPullRequestsRepositoryPullRequestsPageInfo hasNextPage endCursor)
                    (Just projectPRs)
                  )
              )
          ) ->
            let totalCount = Just totalCount'
             in (PageInfo {..}, rateLimit, NoErr, mapMaybe transPR (catMaybes projectPRs))
        Just _ ->
          ( PageInfo False Nothing Nothing
          , Nothing
          , UnknownErr ["Unknown GetProjectPullRequests response: " <> show result]
          , []
          )
        Nothing ->
          ( PageInfo False Nothing Nothing
          , Nothing
          , NoRepo
          , []
          )
  case result of
    GetProjectPullRequests
      (Just (GetProjectPullRequestsRateLimit used remaining (DateTime resetAtText)))
      resp -> process resp (Just $ extractRateLimit used remaining resetAtText)
    GetProjectPullRequests Nothing resp -> process resp Nothing
 where
  getIdent :: Text -> Ident
  getIdent = toIdent host identCB
  getGhostIdent = ghostIdent host

  repoOwnerName :: GetProjectPullRequestsRepositoryPullRequestsNodesRepository -> (Text, Text)
  repoOwnerName
    ( GetProjectPullRequestsRepositoryPullRequestsNodesRepository
        (GetProjectPullRequestsRepositoryPullRequestsNodesRepositoryOwner _ login)
        name
      ) = (login, name)

  repoFullname :: GetProjectPullRequestsRepositoryPullRequestsNodesRepository -> Text
  repoFullname r = let (owner, name) = repoOwnerName r in owner <> "/" <> name

  commitCount :: GetProjectPullRequestsRepositoryPullRequestsNodesCommits -> Int
  commitCount (GetProjectPullRequestsRepositoryPullRequestsNodesCommits count _) = count

  toChangedFiles :: GetProjectPullRequestsRepositoryPullRequestsNodesFiles -> [ChangedFile]
  toChangedFiles (GetProjectPullRequestsRepositoryPullRequestsNodesFiles nodes) =
    toChangedFile <$> catMaybes (fromMaybe [] nodes)
   where
    toChangedFile :: GetProjectPullRequestsRepositoryPullRequestsNodesFilesNodes -> ChangedFile
    toChangedFile GetProjectPullRequestsRepositoryPullRequestsNodesFilesNodes {..} =
      ChangedFile (from additions) (from deletions) (from path)

  toChangeCommits :: GetProjectPullRequestsRepositoryPullRequestsNodesCommits -> [Commit]
  toChangeCommits (GetProjectPullRequestsRepositoryPullRequestsNodesCommits _ nodes) =
    toCommit . commit <$> catMaybes (fromMaybe [] nodes)
   where
    toCommit :: GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommit -> Commit
    toCommit GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommit {..} =
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
        ( GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitAuthor
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitAuthorUser login))
          ) = getIdent login
      getAuthor _ = ghostIdent host
      getCommitter
        ( GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitCommitter
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitCommitterUser login))
          ) = getIdent login
      getCommitter _ = ghostIdent host

  getPRAuthor :: GetProjectPullRequestsRepositoryPullRequestsNodesAuthor -> Text
  getPRAuthor (GetProjectPullRequestsRepositoryPullRequestsNodesAuthor _ login) = login

  getPRMergedBy :: GetProjectPullRequestsRepositoryPullRequestsNodesMergedBy -> Text
  getPRMergedBy (GetProjectPullRequestsRepositoryPullRequestsNodesMergedBy _ login) = login

  getPRMergedCommitSHA :: GetProjectPullRequestsRepositoryPullRequestsNodesMergeCommit -> LText
  getPRMergedCommitSHA (GetProjectPullRequestsRepositoryPullRequestsNodesMergeCommit _ sha) = from $ getSHA sha

  toLabels :: GetProjectPullRequestsRepositoryPullRequestsNodesLabels -> [Text]
  toLabels (GetProjectPullRequestsRepositoryPullRequestsNodesLabels nodes) =
    toLabel <$> catMaybes (fromMaybe [] nodes)
   where
    toLabel :: GetProjectPullRequestsRepositoryPullRequestsNodesLabelsNodes -> Text
    toLabel (GetProjectPullRequestsRepositoryPullRequestsNodesLabelsNodes name) = name

  toAssignees :: GetProjectPullRequestsRepositoryPullRequestsNodesAssignees -> [Ident]
  toAssignees (GetProjectPullRequestsRepositoryPullRequestsNodesAssignees nodes) =
    getIdent . toAssignee <$> catMaybes (fromMaybe [] nodes)
   where
    toAssignee :: GetProjectPullRequestsRepositoryPullRequestsNodesAssigneesNodes -> Text
    toAssignee (GetProjectPullRequestsRepositoryPullRequestsNodesAssigneesNodes login) = login

  getEventsFromTimeline :: Change -> GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItems -> [ChangeEvent]
  getEventsFromTimeline change (GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItems nodes) =
    mapMaybe toEventM (catMaybes (fromMaybe [] nodes))
   where
    toEventM :: GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodes -> Maybe ChangeEvent
    toEventM = \case
      GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesVariantClosedEvent
        ( GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesClosedEvent
            _
            eId
            createdAt
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesActor _ actor))
          ) -> toMaybeMergedOrAbandonedEvent change eId getIdent actor createdAt
      GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesVariantPullRequestReview
        ( GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesPullRequestReview
            _
            eId
            createdAt
            reviewState
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesAuthor _ actor))
          ) -> toMaybeReviewEvent change eId getIdent actor createdAt reviewState
      GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesVariantHeadRefForcePushedEvent
        ( GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesHeadRefForcePushedEvent
            _
            eId
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesFpactor _ actor))
            (Just (GetProjectPullRequestsRepositoryPullRequestsNodesTimelineItemsNodesAfterCommit createdAt))
          ) -> toMaybeForcePushedEvent change eId getIdent actor createdAt
      _ -> Nothing

  getCommitEvents :: Change -> GetProjectPullRequestsRepositoryPullRequestsNodesCommits -> [ChangeEvent]
  getCommitEvents change (GetProjectPullRequestsRepositoryPullRequestsNodesCommits _ nodes) =
    toEvent <$> catMaybes (fromMaybe [] nodes)
   where
    toEvent :: GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodes -> ChangeEvent
    toEvent (GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodes commit) =
      ( baseEvent
          (ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent)
          (from . getSHA $ oid commit)
          change
      )
        { changeEventAuthor = Just $ getCommitter $ committer commit
        , changeEventCreatedAt = Just . from $ committedDate commit
        }
     where
      getCommitter :: Maybe GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitCommitter -> Ident
      getCommitter
        ( Just
            ( GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitCommitter
                (Just (GetProjectPullRequestsRepositoryPullRequestsNodesCommitsNodesCommitCommitterUser login))
              )
          ) = getIdent login
      getCommitter _ = getGhostIdent

  getCommentEvents :: Change -> GetProjectPullRequestsRepositoryPullRequestsNodesComments -> [ChangeEvent]
  getCommentEvents change (GetProjectPullRequestsRepositoryPullRequestsNodesComments nodes) =
    mapMaybe toEvent (catMaybes (fromMaybe [] nodes))
   where
    toEvent :: GetProjectPullRequestsRepositoryPullRequestsNodesCommentsNodes -> Maybe ChangeEvent
    toEvent
      ( GetProjectPullRequestsRepositoryPullRequestsNodesCommentsNodes
          eId
          createdAt
          (Just (GetProjectPullRequestsRepositoryPullRequestsNodesCommentsNodesAuthor _ actor))
        ) = toMaybeCommentEvent change eId getIdent actor createdAt
    toEvent _ = Nothing
  transPR :: GetProjectPullRequestsRepositoryPullRequestsNodes -> Maybe Changes
  transPR GetProjectPullRequestsRepositoryPullRequestsNodes {..} =
    let change =
          Change
            { changeId = from . sanitizeID $ unpackID id
            , changeNumber = from number
            , changeChangeId = getChangeId (repoFullname repository) (show number)
            , changeTitle = from title
            , changeText = from bodyText
            , changeUrl = from . getURL $ webURL
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
            , changeOptionalMergedCommitSha = ChangeOptionalMergedCommitShaMergedCommitSha . getPRMergedCommitSHA <$> mergeCommit
            , changeBranch = from headRefName
            , changeTargetBranch = from baseRefName
            , changeCreatedAt = Just $ from createdAt
            , changeOptionalMergedAt = from <$> mergedAt
            , changeUpdatedAt = Just $ from updatedAt
            , changeOptionalClosedAt = from <$> closedAt
            , changeState = toPRState state
            , changeOptionalDuration = toChangeOptionalDuration state updatedAt closedAt createdAt
            , changeMergeable = from $ toPRMergeableState mergeable
            , changeLabels = fromList $ from <$> maybe [] toLabels labels
            , changeAssignees = fromList $ toAssignees assignees
            , changeApprovals = fromList $ maybe [] ((: []) . from . toApprovals) reviewDecision
            , changeDraft = isDraft
            , changeOptionalSelfMerged =
                Just
                  . ChangeOptionalSelfMergedSelfMerged
                  $ (getPRAuthor <$> author)
                  == (getPRMergedBy <$> mergedBy)
            }
     in Just
          ( change
          , [createdEvent change]
              <> getEventsFromTimeline change timelineItems
              <> getCommitEvents change commits
              <> getCommentEvents change comments
          )
