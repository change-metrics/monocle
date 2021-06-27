{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.MergeRequest where

import Data.Morpheus.Client
import Data.Time.Clock
import Lentille.GitLab (GitLabGraphClient, runGitLabGraphRequest, schemaLocation)
import Lentille.GitLab.Transformer
import Monocle.Change
import Relude hiding (id)

-- TODO
-- How to fetch comments ?
-- Is adding a commit update the updatedAt time ?
-- Need to retreive the State

-- Obs
-- Comment added do not update the updatedAt time
-- MergeBy empty in case of bot user
-- Status open

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
  [gql|
    query GetMergeRequest ($project: ID!, $mergeRequestID: String!) {
      project(fullPath: $project) {
        name
        nameWithNamespace
        namespace {
          name
        }
        mergeRequest (iid: $mergeRequestID) {
          id
          iid
          title
          description
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
          approved
          draft
        }
      }
    }
  |]

-- client <- newGitLabGraphClient "https://gitlab.com/api/graphql"
-- fetchMergeRequest client "redhat/centos-stream/ci-cd/zuul/jobs" 20
fetchMergeRequest :: MonadIO m => GitLabGraphClient -> Text -> Int -> m (Either String GetMergeRequest)
fetchMergeRequest client project mrID =
  fetch (runGitLabGraphRequest client) (GetMergeRequestArgs (ID project) (show mrID))

transformResponse :: GetMergeRequest -> (Change, [ChangeEvent])
transformResponse result =
  case result of
    GetMergeRequest
      ( Just
          ( ProjectProject
              shortName
              fullName
              nameSpaceM
              (Just mr)
            )
        ) -> extract shortName fullName (toNamespaceName nameSpaceM) mr
    otherWise -> error ("Invalid response: " <> show otherwise)
  where
    toNamespaceName nsoM = case nsoM of
      Just nso -> name (nso :: ProjectNamespaceNamespace)
      Nothing -> ""
    extract :: Text -> Text -> Text -> ProjectMergeRequestMergeRequest -> (Change, [ChangeEvent])
    extract shortName fullName namespace mr =
      let change = getChange mr
       in ( change,
            getChangeCreatedEvent change
              <> getChangePushedEvent change
              <> getChangeMergedEvent change
          )
      where
        toDiffStatsSummary ProjectMergeRequestDiffStatsSummaryDiffStatsSummary {..} = DiffStatsSummary {..}
        toDiffStats ProjectMergeRequestDiffStatsDiffStats {..} = DiffStats {..}
        toMRCommit (ProjectMergeRequestCommitsWithoutMergeCommitsNodesCommit sha author' authoredDate ctitle) =
          let cauthor = authorName <$> author' in MRCommit {..}
          where
            authorName ProjectMergeRequestCommitsWithoutMergeCommitsNodesAuthorUserCore {..} = MRUserCore username
        toCommitsNodes
          (ProjectMergeRequestCommitsWithoutMergeCommitsCommitConnection nodes) = cleanMaybeMNodes nodes
        toLabelsNodes
          (ProjectMergeRequestLabelsLabelConnection nodes) = cleanMaybeMNodes nodes
        toAssigneesNodes
          (ProjectMergeRequestAssigneesMergeRequestAssigneeConnection nodes) = cleanMaybeMNodes nodes
        getLabelTitle (ProjectMergeRequestLabelsNodesLabel title') = toLazy title'
        getAuthorUsername ProjectMergeRequestAuthorUserCore {..} = username
        getMergerUsername ProjectMergeRequestMergeUserUserCore {..} = username
        getAssigneesUsername ProjectMergeRequestAssigneesNodesMergeRequestAssignee {..} = username
        getChange :: ProjectMergeRequestMergeRequest -> Change
        getChange ProjectMergeRequestMergeRequest {..} =
          let changeId = (toLazy $ unpackID id)
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
              changeCommits = (fromList $ toCommit . toMRCommit <$> maybe [] toCommitsNodes commitsWithoutMergeCommits)
              changeRepositoryPrefix = toLazy namespace
              changeRepositoryFullname = (toLazy $ removeSpace fullName)
              changeRepositoryShortname = toLazy shortName
              changeAuthor = (Just $ maybe ghostIdent (toIdent . getAuthorUsername) author)
              changeOptionalMergedBy =
                ( Just . ChangeOptionalMergedByMergedBy $
                    maybe ghostIdent (toIdent . getMergerUsername) mergeUser
                )
              changeBranch = toLazy sourceBranch
              changeTargetBranch = toLazy targetBranch
              changeCreatedAt = (Just $ timeToTimestamp Nothing createdAt)
              changeOptionalMergedAt = (ChangeOptionalMergedAtMergedAt . timeToTimestamp Nothing <$> mergedAt)
              changeUpdatedAt = (Just $ timeToTimestamp Nothing updatedAt)
              -- No closedAt attribute for a MR ?
              changeOptionalClosedAt = Nothing
              -- For now unable to get the state https://github.com/morpheusgraphql/morpheus-graphql/issues/600
              changeState = (if isJust mergedAt then "MERGED" else "CLOSED")
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
              changeAssignees = (fromList $ toIdent . getAssigneesUsername <$> maybe [] toAssigneesNodes assignees)
              changeApprovals = (if approved then fromList ["APPROVED"] else fromList [])
              changeDraft = draft
              changeOptionalSelfMerged =
                ( ChangeOptionalSelfMergedSelfMerged
                    <$> ( (==)
                            <$> (getAuthorUsername <$> author) <*> (getMergerUsername <$> mergeUser)
                        )
                )
           in Change {..}

        getBaseEvent :: Change -> ChangeEvent
        getBaseEvent Change {..} =
          let changeEventOnAuthor = changeAuthor
              changeEventOnCreatedAt = changeCreatedAt
              changeEventRepositoryPrefix = changeRepositoryPrefix
              changeEventRepositoryFullname = changeEventRepositoryFullname
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
            | changeState change == "MERGED"
          ]
          where
            getMergedByIdent (ChangeOptionalMergedByMergedBy ident) = ident
            getMergedAt (ChangeOptionalMergedAtMergedAt ts) = ts

        getChangePushedEvent :: Change -> [ChangeEvent]
        getChangePushedEvent change = toList $ mkPushEvent <$> changeCommits change
          where
            mkPushEvent Commit {..} =
              (getBaseEvent change)
                { changeEventId = "ChangePushedEvent-" <> changeId change,
                  changeEventType = Just $ ChangeEventTypeChangePushed ChangePushedEvent,
                  changeEventAuthor = commitAuthor,
                  changeEventCreatedAt = commitAuthoredAt
                }
