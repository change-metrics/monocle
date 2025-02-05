{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.MergeRequests where

import Data.Morpheus.Client
import Data.Text qualified as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Google.Protobuf.Timestamp qualified as T
import Lentille
import Lentille.GitLab.Adapter
import Lentille.GraphQL
import Monocle.Config qualified as Config
import Monocle.Entity
import Monocle.Prelude hiding (id, state)
import Monocle.Protob.Change
import Streaming.Prelude qualified as S

newtype NoteID = NoteID Text deriving (Show, Eq, FromJSON)

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
declareLocalTypesInline
  glSchemaLocation
  [raw|
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

fetchMergeRequest :: GraphEffects es => GraphClient -> Text -> Text -> Eff es (Either (FetchError GetProjectMergeRequests) GetProjectMergeRequests, [RequestLog])
fetchMergeRequest client project mrID =
  fetchWithLog (doGraphRequest client) (GetProjectMergeRequestsArgs (ID project) (Just [mrID]) Nothing)

streamMergeRequests ::
  GraphEffects es =>
  GraphClient ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Config.IdentUG) ->
  UTCTime ->
  Text ->
  LentilleStream es Changes
streamMergeRequests client getIdentIdCb untilDate project =
  streamDropBefore untilDate $ streamFetch client mkArgs defaultStreamFetchOptParams transformResponse'
 where
  mkArgs _ = GetProjectMergeRequestsArgs (ID project) Nothing
  transformResponse' = transformResponse (host client) getIdentIdCb

transformResponse ::
  Text ->
  -- A callback to get Ident ID from an alias
  (Text -> Maybe Config.IdentUG) ->
  GetProjectMergeRequests ->
  (PageInfo, Maybe RateLimit, GraphResponseResult, [(Change, [ChangeEvent])])
transformResponse host getIdentIdCB result =
  case result of
    GetProjectMergeRequests
      ( Just
          ( GetProjectMergeRequestsProject
              shortName
              fullName
              ( Just
                  ( GetProjectMergeRequestsProjectMergeRequests
                      (GetProjectMergeRequestsProjectMergeRequestsPageInfo hasNextPage endCursor)
                      count
                      (Just nodes)
                    )
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor (Just count)
        , Nothing
        , NoErr
        , extract shortName fullName <$> catMaybes nodes
        )
    _anyOtherResponse ->
      ( PageInfo False Nothing Nothing
      , Nothing
      , UnknownErr ["Unknown GetProjectMergeRequests response: " <> show result]
      , []
      )
 where
  toIdent' = toIdent host getIdentIdCB
  toCommit' = toCommit host getIdentIdCB
  extract :: Text -> Text -> GetProjectMergeRequestsProjectMergeRequestsNodes -> (Change, [ChangeEvent])
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
    toDiffStatsSummary GetProjectMergeRequestsProjectMergeRequestsNodesDiffStatsSummary {..} = DiffStatsSummary {..}
    toDiffStats GetProjectMergeRequestsProjectMergeRequestsNodesDiffStats {..} = DiffStats {..}
    toMRCommit (GetProjectMergeRequestsProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodes sha author' authoredDate ctitle) =
      let cauthor = authorName <$> author' in MRCommit {..}
     where
      authorName GetProjectMergeRequestsProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthor {..} = MRUserCore username
    toCommitsNodes
      (GetProjectMergeRequestsProjectMergeRequestsNodesCommitsWithoutMergeCommits nodes) = cleanMaybeMNodes nodes
    toLabelsNodes
      (GetProjectMergeRequestsProjectMergeRequestsNodesLabels nodes) = cleanMaybeMNodes nodes
    toAssigneesNodes
      (GetProjectMergeRequestsProjectMergeRequestsNodesAssignees nodes) = cleanMaybeMNodes nodes
    getLabelTitle (GetProjectMergeRequestsProjectMergeRequestsNodesLabelsNodes title') = from title'
    getAuthorUsername GetProjectMergeRequestsProjectMergeRequestsNodesAuthor {..} = username
    getMergerUsername GetProjectMergeRequestsProjectMergeRequestsNodesMergeUser {..} = username
    getAssigneesUsername GetProjectMergeRequestsProjectMergeRequestsNodesAssigneesNodes {..} = username

    getChange :: GetProjectMergeRequestsProjectMergeRequestsNodes -> Change
    getChange GetProjectMergeRequestsProjectMergeRequestsNodes {..} =
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
            ( Just
                . ChangeOptionalMergedByMergedBy
                $ maybe (ghostIdent host) (toIdent' . getMergerUsername) mergeUser
            )
          changeOptionalMergedCommitSha = Nothing
          changeBranch = from sourceBranch
          changeTargetBranch = from targetBranch
          changeCreatedAt = (Just $ timeToTimestamp createdAt)
          changeOptionalMergedAt = (ChangeOptionalMergedAtMergedAt . timeToTimestamp <$> mergedAt)
          changeUpdatedAt = (Just $ timeToTimestamp updatedAt)
          -- No closedAt attribute for a MR then use updatedAt when the MR is closed state
          changeOptionalClosedAt =
            if isClosed $ toState state
              then Just . ChangeOptionalClosedAtClosedAt $ timeToTimestamp updatedAt
              else Nothing
          changeState = toState state
          changeOptionalDuration = case mergedAt of
            Just merged_ts ->
              Just
                . ChangeOptionalDurationDuration
                . from
                $ diffTimeSec (timeToUTCTime merged_ts) (timeToUTCTime createdAt)
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

    getComments :: GetProjectMergeRequestsProjectMergeRequestsNodes -> [MRComment]
    getComments GetProjectMergeRequestsProjectMergeRequestsNodes {..} =
      toMRComment <$> maybe [] toNotesNodes (Just notes)
     where
      toNotesNodes (GetProjectMergeRequestsProjectMergeRequestsNodesNotes nodes) = cleanMaybeMNodes nodes
      toMRComment :: GetProjectMergeRequestsProjectMergeRequestsNodesNotesNodes -> MRComment
      toMRComment (GetProjectMergeRequestsProjectMergeRequestsNodesNotesNodes nId author' commentedAt ntypeM) =
        let commentType = getCommentType ntypeM
            NoteID noteIDT = nId
            noteIdent = case author' of
              Just (GetProjectMergeRequestsProjectMergeRequestsNodesNotesNodesAuthor author''') -> toIdent' author'''
              Nothing -> Lentille.ghostIdent host
         in MRComment
              { coId = sanitizeID noteIDT
              , coAuthor = noteIdent
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
          changeEventDraft = changeDraft
          -- To be filled by caller function
          changeEventCreatedAt = Nothing
          changeEventAuthor = Nothing
          changeEventType = Nothing
          changeEventLabels = changeLabels
          changeEventOptionalDuration = swapDuration <$> changeOptionalDuration
          changeEventId = ""
          changeEventOptionalMergedCommitSha = Nothing
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
          , changeEventCreatedAt = Just $ timeToTimestamp coAuthoredAt
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
              , changeEventCreatedAt = Just $ timeToTimestamp coAuthoredAt
              }
