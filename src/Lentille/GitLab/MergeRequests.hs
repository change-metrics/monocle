{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.MergeRequests where

import Data.Morpheus.Client
import Data.Text qualified as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import Data.Vector qualified as V
import Google.Protobuf.Timestamp qualified as T
import Lentille
import Lentille.GitLab.Adapter
import Lentille.GraphQL
import Monocle.Logging (Entity (Project), LogCrawlerContext, noContext)
import Monocle.Prelude hiding (id, state)
import Monocle.Protob.Change
import Streaming.Prelude qualified as S

newtype NoteID = NoteID Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  glSchemaLocation
  [gql|
    query GetProjectMergeRequests ($project: ID!, $iids: [String!], $cursor: String) {
      project(fullPath: $project) {
        name
        nameWithNamespace
        mergeRequests (first: 100, after: $cursor, sort: UPDATED_DESC, iids: $iids) {
          pageInfo {hasNextPage endCursor}
          count
          nodes {
            id
            iid
            title
            description
            state
            webUrl
            commitCount
            diffStatsSummary {
              additions
              deletions
              fileCount
            }
            diffStats {
              path
              additions
              deletions
            }
            commitsWithoutMergeCommits (first: 100) {
              nodes {
                sha
                author {
                  username
                }
                authoredDate
                title
              }
            }
            author {
              username
            }
            mergeUser {
              username
            }
            sourceBranch
            targetBranch
            createdAt
            updatedAt
            mergedAt
            mergeable
            labels {
              nodes {
                title
              }
            }
            assignees {
              nodes {
                username
              }
            }
            notes {
              nodes {
                id
                author {
                  username
                }
                createdAt
                systemNoteIconName
              }
            }
            draft
          }
        }
      }
    }
  |]

type Changes = (Change, [ChangeEvent])

fetchMergeRequest :: MonadGraphQL m => GraphClient -> Text -> Text -> m (Either (FetchError GetProjectMergeRequests) GetProjectMergeRequests, [RequestLog])
fetchMergeRequest client project mrID =
  fetchWithLog (doGraphRequest noContext client) (GetProjectMergeRequestsArgs (ID project) (Just [mrID]) Nothing)

streamMergeRequests ::
  MonadGraphQLE m =>
  GraphClient ->
  (Entity -> LogCrawlerContext) ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  UTCTime ->
  Text ->
  LentilleStream m Changes
streamMergeRequests client mkLC getIdentIdCb untilDate project =
  breakOnDate $ streamFetch client (mkLC $ Project project) mkArgs defaultStreamFetchOptParams transformResponse'
 where
  mkArgs _ = GetProjectMergeRequestsArgs (ID project) Nothing

  -- This transform the stream by adding a limit.
  -- We don't care about the rest so we replace it with ()
  breakOnDate = fmap (pure ()) . S.break (isChangeTooOld untilDate)

  transformResponse' = transformResponse (host client) getIdentIdCb

transformResponse ::
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Text) ->
  GetProjectMergeRequests ->
  (PageInfo, Maybe RateLimit, [Text], [(Change, [ChangeEvent])])
transformResponse host getIdentIdCB result =
  case result of
    GetProjectMergeRequests
      ( Just
          ( ProjectProject
              shortName
              fullName
              ( Just
                  ( ProjectMergeRequestsMergeRequestConnection
                      (ProjectMergeRequestsPageInfoPageInfo hasNextPage endCursor)
                      count
                      (Just nodes)
                    )
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor (Just count)
        , Nothing
        , []
        , extract shortName fullName <$> catMaybes nodes
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing
      , Nothing
      , ["Unknown GetProjectMergeRequests response: " <> show result]
      , []
      )
 where
  toIdent' = toIdent host getIdentIdCB
  toCommit' = toCommit host getIdentIdCB
  extract :: Text -> Text -> ProjectMergeRequestsNodesMergeRequest -> (Change, [ChangeEvent])
  extract shortName fullName mr =
    let change = getChange mr
        comments = getComments mr
     in ( change
        , getChangeCreatedEvent change
            <> getChangePushedEvent change
            <> getChangeMergedEvent change
            <> getChangeAbandonedEvent change
            <> getChangeCommentedEvent change comments
            <> getChangeReviewedEvent change comments
        )
   where
    toDiffStatsSummary ProjectMergeRequestsNodesDiffStatsSummaryDiffStatsSummary {..} = DiffStatsSummary {..}
    toDiffStats ProjectMergeRequestsNodesDiffStatsDiffStats {..} = DiffStats {..}
    toMRCommit (ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesCommit sha author' authoredDate ctitle) =
      let cauthor = authorName <$> author' in MRCommit {..}
     where
      authorName ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthorUserCore {..} = MRUserCore username
    toCommitsNodes
      (ProjectMergeRequestsNodesCommitsWithoutMergeCommitsCommitConnection nodes) = cleanMaybeMNodes nodes
    toLabelsNodes
      (ProjectMergeRequestsNodesLabelsLabelConnection nodes) = cleanMaybeMNodes nodes
    toAssigneesNodes
      (ProjectMergeRequestsNodesAssigneesMergeRequestAssigneeConnection nodes) = cleanMaybeMNodes nodes
    getLabelTitle (ProjectMergeRequestsNodesLabelsNodesLabel title') = from title'
    getAuthorUsername ProjectMergeRequestsNodesAuthorUserCore {..} = username
    getMergerUsername ProjectMergeRequestsNodesMergeUserUserCore {..} = username
    getAssigneesUsername ProjectMergeRequestsNodesAssigneesNodesMergeRequestAssignee {..} = username

    getChange :: ProjectMergeRequestsNodesMergeRequest -> Change
    getChange ProjectMergeRequestsNodesMergeRequest {..} =
      let changeId = (from . sanitizeID $ unpackID id)
          changeNumber = getChangeNumber iid
          changeChangeId = getChangeId fullName iid
          changeTitle = from title
          changeText = fromMTtoLT description
          changeUrl = fromMTtoLT webUrl
          changeCommitCount = (from $ fromMaybe 0 commitCount)
          changeAdditions = (from $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSAdditions)
          changeDeletions = (from $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSDeletions)
          changeChangedFilesCount = (from $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSFileCount)
          changeChangedFiles = (fromList $ getChangedFile . toDiffStats <$> fromMaybe [] diffStats)
          changeCommits = (fromList $ toCommit' . toMRCommit <$> maybe [] toCommitsNodes commitsWithoutMergeCommits)
          changeRepositoryPrefix = from $ TE.replace ("/" <> shortName) "" $ stripSpaces fullName
          changeRepositoryFullname = from $ stripSpaces fullName
          changeRepositoryShortname = from shortName
          changeAuthor = Just (maybe (ghostIdent host) (toIdent' . getAuthorUsername) author)
          changeOptionalMergedBy =
            ( Just . ChangeOptionalMergedByMergedBy $
                maybe (ghostIdent host) (toIdent' . getMergerUsername) mergeUser
            )
          changeBranch = from sourceBranch
          changeTargetBranch = from targetBranch
          changeCreatedAt = (Just $ timeToTimestamp Nothing createdAt)
          changeOptionalMergedAt = (ChangeOptionalMergedAtMergedAt . timeToTimestamp Nothing <$> mergedAt)
          changeUpdatedAt = (Just $ timeToTimestamp Nothing updatedAt)
          -- No closedAt attribute for a MR then use updatedAt when the MR is closed state
          changeOptionalClosedAt =
            if isClosed $ toState state
              then Just . ChangeOptionalClosedAtClosedAt $ timeToTimestamp Nothing updatedAt
              else Nothing
          changeState = toState state
          changeOptionalDuration = case mergedAt of
            Just merged_ts ->
              Just
                . ChangeOptionalDurationDuration
                . from
                $ diffTimeSec (timeToUTCTime Nothing merged_ts) (timeToUTCTime Nothing createdAt)
            Nothing -> Nothing

          -- TODO(fbo) Use the merge status : https://docs.gitlab.com/ee/api/graphql/reference/index.html#mergestatus
          changeMergeable = (if mergeable then "MERGEABLE" else "CONFLICT")
          changeLabels = (fromList $ getLabelTitle <$> maybe [] toLabelsNodes labels)
          changeAssignees = (fromList $ toIdent' . getAssigneesUsername <$> maybe [] toAssigneesNodes assignees)
          -- GitLab 13.12.X does not expose an approval attribute for mergeRequest
          -- changeApprovals = (if approved then fromList ["APPROVED"] else fromList [])
          changeApprovals = fromList []
          changeDraft = draft
          changeOptionalSelfMerged =
            ( ChangeOptionalSelfMergedSelfMerged
                <$> ( (==)
                        <$> (getAuthorUsername <$> author)
                        <*> (getMergerUsername <$> mergeUser)
                    )
            )
       in Change {..}

    getComments :: ProjectMergeRequestsNodesMergeRequest -> [MRComment]
    getComments ProjectMergeRequestsNodesMergeRequest {..} =
      toMRComment <$> maybe [] toNotesNodes (Just notes)
     where
      toNotesNodes (ProjectMergeRequestsNodesNotesNoteConnection nodes) = cleanMaybeMNodes nodes
      toMRComment :: ProjectMergeRequestsNodesNotesNodesNote -> MRComment
      toMRComment (ProjectMergeRequestsNodesNotesNodesNote nId author' commentedAt ntypeM) =
        let ProjectMergeRequestsNodesNotesNodesAuthorUserCore author'' = author'
            commentType = getCommentType ntypeM
            NoteID noteIDT = nId
         in MRComment
              { coId = sanitizeID noteIDT
              , coAuthor = toIdent' author''
              , coAuthoredAt = commentedAt
              , coType = commentType
              }
      getCommentType = \case
        Just "approval" -> CoApproval "Approved"
        Just "unapproval" -> CoApproval "Unapproved"
        Just _ -> CoOther
        Nothing -> CoComment

    getBaseEvent :: Change -> ChangeEvent
    getBaseEvent Change {..} =
      let changeEventOnAuthor = changeAuthor
          changeEventOnCreatedAt = changeCreatedAt
          changeEventRepositoryPrefix = changeRepositoryPrefix
          changeEventRepositoryFullname = changeRepositoryFullname
          changeEventRepositoryShortname = changeRepositoryShortname
          changeEventBranch = changeBranch
          changeEventTargetBranch = changeTargetBranch
          changeEventNumber = changeNumber
          changeEventChangeId = changeChangeId
          changeEventUrl = changeUrl
          changeEventChangedFiles = fmap toChangeFilePath changeChangedFiles
          -- To be filled by caller function
          changeEventCreatedAt = Nothing
          changeEventAuthor = Nothing
          changeEventType = Nothing
          changeEventLabels = changeLabels
          changeEventOptionalDuration = swapDuration <$> changeOptionalDuration
          changeEventId = ""
       in ChangeEvent {..}
     where
      toChangeFilePath ChangedFile {..} = ChangedFilePath changedFilePath

    getChangeCreatedEvent :: Change -> [ChangeEvent]
    getChangeCreatedEvent change =
      [ (getBaseEvent change)
          { changeEventId = "ChangeCreatedEvent-" <> changeId change
          , changeEventType = Just $ ChangeEventTypeChangeCreated ChangeCreatedEvent
          , changeEventAuthor = changeAuthor change
          , changeEventCreatedAt = changeCreatedAt change
          }
      ]

    getChangeMergedEvent :: Change -> [ChangeEvent]
    getChangeMergedEvent change =
      [ (getBaseEvent change)
        { changeEventId = "ChangeMergedEvent-" <> changeId change
        , changeEventType = Just $ ChangeEventTypeChangeMerged ChangeMergedEvent
        , changeEventAuthor = getMergedByIdent <$> changeOptionalMergedBy change
        , changeEventCreatedAt = getMergedAt <$> changeOptionalMergedAt change
        }
      | isMerged $ changeState change
      ]
     where
      getMergedByIdent (ChangeOptionalMergedByMergedBy ident) = ident
      getMergedAt (ChangeOptionalMergedAtMergedAt ts) = ts

    getChangeAbandonedEvent :: Change -> [ChangeEvent]
    getChangeAbandonedEvent change =
      [ (getBaseEvent change)
        { changeEventId = "ChangeAbandonedEvent-" <> changeId change
        , changeEventType = Just $ ChangeEventTypeChangeAbandoned ChangeAbandonedEvent
        , -- GitLab Graph API does not report who closed a MR
          changeEventAuthor = changeAuthor change
        , changeEventCreatedAt = changeUpdatedAt change
        }
      | isClosed $ changeState change
      ]

    getChangePushedEvent :: Change -> [ChangeEvent]
    getChangePushedEvent change = toList $ mkPushEvent <$> changeCommits change
     where
      mkPushEvent Commit {..} =
        (getBaseEvent change)
          { changeEventId = "ChangeCommitPushedEvent-" <> changeId change <> "-" <> commitSha
          , changeEventType = Just $ ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent
          , changeEventAuthor = commitAuthor
          , changeEventCreatedAt = commitAuthoredAt
          }

    getChangeCommentedEvent :: Change -> [MRComment] -> [ChangeEvent]
    getChangeCommentedEvent change comments =
      toList $ mkCommentEvent <$> filter isComment comments
     where
      mkCommentEvent MRComment {..} =
        (getBaseEvent change)
          { changeEventId = "ChangeCommentedEvent-" <> changeId change <> "-" <> from coId
          , changeEventType = Just $ ChangeEventTypeChangeCommented ChangeCommentedEvent
          , changeEventAuthor = Just coAuthor
          , changeEventCreatedAt = Just $ timeToTimestamp Nothing coAuthoredAt
          }

    getChangeReviewedEvent :: Change -> [MRComment] -> [ChangeEvent]
    getChangeReviewedEvent change comments =
      toList $ mkCommentEvent <$> filter isApprovalComment comments
     where
      mkCommentEvent MRComment {..} =
        let approval = case coType of
              CoApproval approval' -> approval'
              _ -> error "Runtime error"
         in (getBaseEvent change)
              { changeEventId = "ChangeReviewedEvent-" <> changeId change <> "-" <> from coId
              , changeEventType =
                  Just
                    . ChangeEventTypeChangeReviewed
                    . ChangeReviewedEvent
                    $ fromList [from approval]
              , changeEventAuthor = Just coAuthor
              , changeEventCreatedAt = Just $ timeToTimestamp Nothing coAuthoredAt
              }
