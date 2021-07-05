{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports -Wno-orphans #-}

module Lentille.GitLab.MergeRequests where

import Data.Morpheus.Client
import qualified Data.Text as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Data.Vector as V
import qualified Google.Protobuf.Timestamp as T
import Lentille.GitLab
  ( GitLabGraphClient,
    PageInfo (..),
    host,
    newGitLabGraphClient,
    runGitLabGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Lentille.GitLab.Adapter
import Monocle.Change
import Relude hiding (id, state)
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

newtype NoteID = NoteID Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
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
            approved
            draft
          }
        }
      }
    }
  |]

type Changes = (Change, [ChangeEvent])

fetchMergeRequest :: MonadIO m => GitLabGraphClient -> Text -> String -> m (Either String GetProjectMergeRequests)
fetchMergeRequest client project mrID =
  fetch (runGitLabGraphRequest client) (GetProjectMergeRequestsArgs (ID project) (Just [mrID]) Nothing)

streamMergeRequests ::
  MonadIO m => GitLabGraphClient -> UTCTime -> Text -> Stream (Of Changes) m ()
streamMergeRequests client untilDate project =
  streamFetch client (Just untilDate) mkArgs (transformResponse $ host client) breakOnDate
  where
    mkArgs cursor = GetProjectMergeRequestsArgs (ID project) Nothing $ toCursorM cursor
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just . toString $ cursor''

    -- This transform the stream by adding a limit.
    -- We don't care about the rest so we replace it with ()
    breakOnDate = fmap (pure ()) . S.break isChangeTooOld

    isChangeTooOld :: Changes -> Bool
    isChangeTooOld (change, _) =
      case changeUpdatedAt change of
        Just changeDate -> isDateOlderThan (T.toUTCTime changeDate) untilDate
        _ -> True

    -- t1 is older than t2 then return True
    isDateOlderThan :: UTCTime -> UTCTime -> Bool
    isDateOlderThan t1 t2 = diffUTCTime t1 t2 < 0

transformResponse :: Text -> GetProjectMergeRequests -> (PageInfo, [Text], [(Change, [ChangeEvent])])
transformResponse host result =
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
        ( PageInfo hasNextPage endCursor (Just count),
          [],
          extract shortName fullName <$> catMaybes nodes
        )
    otherWise -> error ("Invalid response: " <> show otherwise)
  where
    extract :: Text -> Text -> ProjectMergeRequestsNodesMergeRequest -> (Change, [ChangeEvent])
    extract shortName fullName mr =
      let change = getChange mr
          comments = getComments mr
       in ( change,
            getChangeCreatedEvent change
              <> getChangePushedEvent change
              <> getChangeMergedEvent change
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
        getLabelTitle (ProjectMergeRequestsNodesLabelsNodesLabel title') = toLazy title'
        getAuthorUsername ProjectMergeRequestsNodesAuthorUserCore {..} = username
        getMergerUsername ProjectMergeRequestsNodesMergeUserUserCore {..} = username
        getAssigneesUsername ProjectMergeRequestsNodesAssigneesNodesMergeRequestAssignee {..} = username

        getChange :: ProjectMergeRequestsNodesMergeRequest -> Change
        getChange ProjectMergeRequestsNodesMergeRequest {..} =
          let changeId = (toLazy . sanitizeID $ unpackID id)
              changeNumber = getChangeNumber iid
              changeChangeId = getChangeId fullName iid
              changeTitle = toLazy title
              changeText = fromMTtoLT description
              changeUrl = fromMTtoLT webUrl
              changeCommitCount = (fromIntToInt32 $ fromMaybe 0 commitCount)
              changeAdditions = (fromIntToInt32 $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSAdditions)
              changeDeletions = (fromIntToInt32 $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSDeletions)
              changeChangedFilesCount = (fromIntToInt32 $ getDSS (toDiffStatsSummary <$> diffStatsSummary) DSSFileCount)
              changeChangedFiles = (fromList $ getChangedFile . toDiffStats <$> fromMaybe [] diffStats)
              changeCommits = (fromList $ toCommit host . toMRCommit <$> maybe [] toCommitsNodes commitsWithoutMergeCommits)
              changeRepositoryPrefix = toLazy $ TE.replace ("/" <> shortName) "" $ removeSpace fullName
              changeRepositoryFullname = toLazy $ removeSpace fullName
              changeRepositoryShortname = toLazy shortName
              changeAuthor = (Just $ maybe (ghostIdent host) (toIdent host . getAuthorUsername) author)
              changeOptionalMergedBy =
                ( Just . ChangeOptionalMergedByMergedBy $
                    maybe (ghostIdent host) (toIdent host . getMergerUsername) mergeUser
                )
              changeBranch = toLazy sourceBranch
              changeTargetBranch = toLazy targetBranch
              changeCreatedAt = (Just $ timeToTimestamp Nothing createdAt)
              changeOptionalMergedAt = (ChangeOptionalMergedAtMergedAt . timeToTimestamp Nothing <$> mergedAt)
              changeUpdatedAt = (Just $ timeToTimestamp Nothing updatedAt)
              -- No closedAt attribute for a MR then use updatedAt when the MR is closed state
              changeOptionalClosedAt =
                if isClosed $ toState state
                  then Just . ChangeOptionalClosedAtClosedAt $ timeToTimestamp Nothing updatedAt
                  else Nothing
              changeState = toState state
              changeOptionalDuration =
                ( ChangeOptionalDurationDuration . fromIntToInt32
                    . diffTime
                      ( timeToUTCTime Nothing createdAt
                      )
                    <$> (timeToUTCTime Nothing <$> mergedAt)
                )
              -- TODO(fbo) Use the merge status : https://docs.gitlab.com/ee/api/graphql/reference/index.html#mergestatus
              changeMergeable = (if mergeable then "MERGEABLE" else "CONFLICT")
              changeLabels = (fromList $ getLabelTitle <$> maybe [] toLabelsNodes labels)
              changeAssignees = (fromList $ toIdent host . getAssigneesUsername <$> maybe [] toAssigneesNodes assignees)
              changeApprovals = (if approved then fromList ["APPROVED"] else fromList [])
              changeDraft = draft
              changeOptionalSelfMerged =
                ( ChangeOptionalSelfMergedSelfMerged
                    <$> ( (==)
                            <$> (getAuthorUsername <$> author) <*> (getMergerUsername <$> mergeUser)
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
                    { coId = sanitizeID noteIDT,
                      coAuthor = toIdent host author'',
                      coAuthoredAt = commentedAt,
                      coType = commentType
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
              changeEventId = ""
           in ChangeEvent {..}
          where
            toChangeFilePath ChangedFile {..} = ChangedFilePath changedFilePath

        getChangeCreatedEvent :: Change -> [ChangeEvent]
        getChangeCreatedEvent change =
          [ (getBaseEvent change)
              { changeEventId = "ChangeCreatedEvent-" <> changeId change,
                changeEventType = Just $ ChangeEventTypeChangeCreated ChangeCreatedEvent,
                changeEventAuthor = changeAuthor change,
                changeEventCreatedAt = changeCreatedAt change
              }
          ]

        getChangeMergedEvent :: Change -> [ChangeEvent]
        getChangeMergedEvent change =
          [ (getBaseEvent change)
              { changeEventId = "ChangeMergedEvent-" <> changeId change,
                changeEventType = Just $ ChangeEventTypeChangeMerged ChangeMergedEvent,
                changeEventAuthor = getMergedByIdent <$> changeOptionalMergedBy change,
                changeEventCreatedAt = getMergedAt <$> changeOptionalMergedAt change
              }
            | isMerged $ changeState change
          ]
          where
            getMergedByIdent (ChangeOptionalMergedByMergedBy ident) = ident
            getMergedAt (ChangeOptionalMergedAtMergedAt ts) = ts

        getChangePushedEvent :: Change -> [ChangeEvent]
        getChangePushedEvent change = toList $ mkPushEvent <$> changeCommits change
          where
            mkPushEvent Commit {..} =
              (getBaseEvent change)
                { changeEventId = "ChangeCommitPushedEvent-" <> changeId change <> "-" <> commitSha,
                  changeEventType = Just $ ChangeEventTypeChangeCommitPushed ChangeCommitPushedEvent,
                  changeEventAuthor = commitAuthor,
                  changeEventCreatedAt = commitAuthoredAt
                }

        getChangeCommentedEvent :: Change -> [MRComment] -> [ChangeEvent]
        getChangeCommentedEvent change comments =
          toList $ mkCommentEvent <$> filter isComment comments
          where
            mkCommentEvent MRComment {..} =
              (getBaseEvent change)
                { changeEventId = "ChangeCommentedEvent-" <> changeId change <> "-" <> toLazy coId,
                  changeEventType = Just $ ChangeEventTypeChangeCommented ChangeCommentedEvent,
                  changeEventAuthor = Just coAuthor,
                  changeEventCreatedAt = Just $ timeToTimestamp Nothing coAuthoredAt
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
                    { changeEventId = "ChangeReviewedEvent-" <> changeId change <> "-" <> toLazy coId,
                      changeEventType =
                        Just
                          . ChangeEventTypeChangeReviewed
                          . ChangeReviewedEvent
                          $ fromList [toLazy approval],
                      changeEventAuthor = Just coAuthor,
                      changeEventCreatedAt = Just $ timeToTimestamp Nothing coAuthoredAt
                    }
