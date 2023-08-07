{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lentille.GitHub.UserPullRequests where

import Data.Morpheus.Client
import Lentille
import Lentille.GitHub.GraphQLFragments (fragmentPRData)
import Lentille.GitHub.Types
import Lentille.GitHub.Utils
import Lentille.GraphQL
import Monocle.Prelude hiding (id, state)
import Monocle.Protob.Change
import Streaming.Prelude qualified as S (break)

-- https://docs.github.com/en/graphql/reference/queries#user
declareLocalTypesInline
  ghSchemaLocation
  ( [raw|
    query GetUserPullRequests ($login: String!, $depth: Int, $cursor: String) {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $login) {
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

streamUserPullRequests ::
  GraphEffects es =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  UTCTime ->
  Text ->
  LentilleStream es Changes
streamUserPullRequests client cb untilDate userLogin =
  breakOnDate $ streamFetch client mkArgs optParams transformResponse'
 where
  mkArgs = GetUserPullRequestsArgs userLogin
  transformResponse' = transformResponse (getHost client) cb
  -- This transform the stream by adding a limit.
  -- We don't care about the rest so we replace it with ()
  breakOnDate = fmap (pure ()) . S.break (isChangeTooOld untilDate)

transformResponse ::
  -- hostname of the provider
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  -- The response payload
  GetUserPullRequests ->
  (PageInfo, Maybe RateLimit, [Text], [Changes])
transformResponse host identCB result = do
  let process resp rateLimit = case resp of
        ( Just
            ( GetUserPullRequestsUser
                ( GetUserPullRequestsUserPullRequests
                    totalCount'
                    (GetUserPullRequestsUserPullRequestsPageInfo hasNextPage endCursor)
                    (Just projectPRs)
                  )
              )
          ) ->
            let totalCount = Just totalCount'
             in (PageInfo {..}, rateLimit, [], mapMaybe transPR (catMaybes projectPRs))
        _anyOtherResponse ->
          ( PageInfo False Nothing Nothing
          , Nothing
          , ["Unknown GetUserPullRequests response: " <> show result]
          , []
          )
  case result of
    GetUserPullRequests
      (Just (GetUserPullRequestsRateLimit used remaining (DateTime resetAtText)))
      resp -> process resp (Just $ extractRateLimit used remaining resetAtText)
    GetUserPullRequests Nothing resp -> process resp Nothing
 where
  getIdent :: Text -> Ident
  getIdent = toIdent host identCB
  getGhostIdent = ghostIdent host

  repoOwnerName :: GetUserPullRequestsUserPullRequestsNodesRepository -> (Text, Text)
  repoOwnerName
    ( GetUserPullRequestsUserPullRequestsNodesRepository
        (GetUserPullRequestsUserPullRequestsNodesRepositoryOwner _ login)
        name
      ) = (login, name)

  repoFullname :: GetUserPullRequestsUserPullRequestsNodesRepository -> Text
  repoFullname r = let (owner, name) = repoOwnerName r in owner <> "/" <> name

  commitCount :: GetUserPullRequestsUserPullRequestsNodesCommits -> Int
  commitCount (GetUserPullRequestsUserPullRequestsNodesCommits count _) = count

  toChangedFiles :: GetUserPullRequestsUserPullRequestsNodesFiles -> [ChangedFile]
  toChangedFiles (GetUserPullRequestsUserPullRequestsNodesFiles nodes) =
    toChangedFile <$> catMaybes (fromMaybe [] nodes)
   where
    toChangedFile :: GetUserPullRequestsUserPullRequestsNodesFilesNodes -> ChangedFile
    toChangedFile GetUserPullRequestsUserPullRequestsNodesFilesNodes {..} =
      ChangedFile (from additions) (from deletions) (from path)

  toChangeCommits :: GetUserPullRequestsUserPullRequestsNodesCommits -> [Commit]
  toChangeCommits (GetUserPullRequestsUserPullRequestsNodesCommits _ nodes) =
    toCommit . commit <$> catMaybes (fromMaybe [] nodes)
   where
    toCommit :: GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommit -> Commit
    toCommit GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommit {..} =
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
        ( GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitAuthor
            (Just (GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitAuthorUser login))
          ) = getIdent login
      getAuthor _ = ghostIdent host
      getCommitter
        ( GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitCommitter
            (Just (GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitCommitterUser login))
          ) = getIdent login
      getCommitter _ = ghostIdent host

  getPRAuthor :: GetUserPullRequestsUserPullRequestsNodesAuthor -> Text
  getPRAuthor (GetUserPullRequestsUserPullRequestsNodesAuthor _ login) = login

  getPRMergedBy :: GetUserPullRequestsUserPullRequestsNodesMergedBy -> Text
  getPRMergedBy (GetUserPullRequestsUserPullRequestsNodesMergedBy _ login) = login

  toLabels :: GetUserPullRequestsUserPullRequestsNodesLabels -> [Text]
  toLabels (GetUserPullRequestsUserPullRequestsNodesLabels nodes) =
    toLabel <$> catMaybes (fromMaybe [] nodes)
   where
    toLabel :: GetUserPullRequestsUserPullRequestsNodesLabelsNodes -> Text
    toLabel (GetUserPullRequestsUserPullRequestsNodesLabelsNodes name) = name

  toAssignees :: GetUserPullRequestsUserPullRequestsNodesAssignees -> [Ident]
  toAssignees (GetUserPullRequestsUserPullRequestsNodesAssignees nodes) =
    getIdent . toAssignee <$> catMaybes (fromMaybe [] nodes)
   where
    toAssignee :: GetUserPullRequestsUserPullRequestsNodesAssigneesNodes -> Text
    toAssignee (GetUserPullRequestsUserPullRequestsNodesAssigneesNodes login) = login

  getEventsFromTimeline :: Change -> GetUserPullRequestsUserPullRequestsNodesTimelineItems -> [ChangeEvent]
  getEventsFromTimeline change (GetUserPullRequestsUserPullRequestsNodesTimelineItems nodes) =
    mapMaybe toEventM (catMaybes (fromMaybe [] nodes))
   where
    toEventM :: GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodes -> Maybe ChangeEvent
    toEventM = \case
      GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesVariantClosedEvent
        ( GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesClosedEvent
            _
            eId
            createdAt
            (Just (GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesActor _ actor))
          ) -> toMaybeMergedOrAbandonedEvent change eId getIdent actor createdAt
      GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesVariantPullRequestReview
        ( GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesPullRequestReview
            _
            eId
            createdAt
            reviewState
            (Just (GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesAuthor _ actor))
          ) -> toMaybeReviewEvent change eId getIdent actor createdAt reviewState
      GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesVariantHeadRefForcePushedEvent
        ( GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesHeadRefForcePushedEvent
            _
            eId
            createdAt
            (Just (GetUserPullRequestsUserPullRequestsNodesTimelineItemsNodesFpactor _ actor))
          ) -> toMaybeForcePushedEvent change eId getIdent actor createdAt
      _ -> Nothing

  getCommitEvents :: Change -> GetUserPullRequestsUserPullRequestsNodesCommits -> [ChangeEvent]
  getCommitEvents change (GetUserPullRequestsUserPullRequestsNodesCommits _ nodes) =
    toEvent <$> catMaybes (fromMaybe [] nodes)
   where
    toEvent :: GetUserPullRequestsUserPullRequestsNodesCommitsNodes -> ChangeEvent
    toEvent (GetUserPullRequestsUserPullRequestsNodesCommitsNodes commit) =
      ( baseEvent
          (ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent)
          (from . getSHA $ oid commit)
          change
      )
        { changeEventAuthor = Just $ getCommitter $ committer commit
        , changeEventCreatedAt = maybe (changeCreatedAt change) (Just . from) (pushedDate commit)
        }
     where
      getCommitter :: Maybe GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitCommitter -> Ident
      getCommitter
        ( Just
            ( GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitCommitter
                (Just (GetUserPullRequestsUserPullRequestsNodesCommitsNodesCommitCommitterUser login))
              )
          ) = getIdent login
      getCommitter _ = getGhostIdent

  getCommentEvents :: Change -> GetUserPullRequestsUserPullRequestsNodesComments -> [ChangeEvent]
  getCommentEvents change (GetUserPullRequestsUserPullRequestsNodesComments nodes) =
    mapMaybe toEvent (catMaybes (fromMaybe [] nodes))
   where
    toEvent :: GetUserPullRequestsUserPullRequestsNodesCommentsNodes -> Maybe ChangeEvent
    toEvent
      ( GetUserPullRequestsUserPullRequestsNodesCommentsNodes
          eId
          createdAt
          (Just (GetUserPullRequestsUserPullRequestsNodesCommentsNodesAuthor _ actor))
        ) = toMaybeCommentEvent change eId getIdent actor createdAt
    toEvent _ = Nothing
  transPR :: GetUserPullRequestsUserPullRequestsNodes -> Maybe Changes
  transPR GetUserPullRequestsUserPullRequestsNodes {..} =
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
