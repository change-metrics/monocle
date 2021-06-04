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

module Lentille.GitLab.MergeRequests where

import Data.Morpheus.Client
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Data.Vector as V
import qualified Google.Protobuf.Timestamp as T
import Lentille.GitLab
  ( GitLabGraphClient,
    PageInfo (..),
    newGitLabGraphClient,
    runGitLabGraphRequest,
    schemaLocation,
    streamFetch,
  )
import Monocle.Change
import Relude hiding (id)
import Streaming (Of, Stream)

-- TODO(fbo): use NamedFieldPuns

newtype Time = Time Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- data MergeRequestState = MergeRequestStateOpened | MergeRequestStateClosed | MergeRequestStateLocked | MergeRequestStateAll | MergeRequestStateMerged

-- https://docs.gitlab.com/ee/api/graphql/reference/index.html#projectmergerequests
defineByDocumentFile
  schemaLocation
  [gql|
    query GetProjectMergeRequests ($project: ID!, $cursor: String) {
      project(fullPath: $project) {
        name
        nameWithNamespace
        namespace {
          name
        }
        mergeRequests (first: 100, after: $cursor, sort: UPDATED_DESC) {
          pageInfo {hasNextPage endCursor}
          count
          nodes {
            id
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
            mergeUser {
              username
            }
          }
        }
      }
    }
  |]

fetchMergeRequests :: MonadIO m => GitLabGraphClient -> String -> m (Either String GetProjectMergeRequests)
fetchMergeRequests client project =
  fetch (runGitLabGraphRequest client) (GetProjectMergeRequestsArgs (toProjectID project) Nothing)

toProjectID :: String -> ID
toProjectID project' = ID $ toText project'

streamMergeRequests :: MonadIO m => GitLabGraphClient -> UTCTime -> String -> Stream (Of Change) m ()
streamMergeRequests client untilDate project =
  streamFetch client untilDate mkArgs transformResponse
  where
    mkArgs cursor = GetProjectMergeRequestsArgs (toProjectID project) $ toCursorM cursor
    toCursorM :: Text -> Maybe String
    toCursorM "" = Nothing
    toCursorM cursor'' = Just . toString $ cursor''

transformResponse :: GetProjectMergeRequests -> (PageInfo, [Text], [Change])
transformResponse result =
  case result of
    GetProjectMergeRequests
      ( Just
          ( ProjectProject
              shortName
              fullName
              nameSpaceM
              ( Just
                  ( ProjectMergeRequestsMergeRequestConnection
                      (ProjectMergeRequestsPageInfoPageInfo hasNextPage endCursor)
                      count
                      (Just nodes)
                    )
                )
            )
        ) ->
        ( PageInfo hasNextPage endCursor count,
          [],
          nodesToChanges shortName fullName (toNamespaceName nameSpaceM) nodes
        )
    otherWise -> error ("Invalid response: " <> show otherwise)
  where
    toNamespaceName nsoM = case nsoM of
      Just nso -> name (nso :: ProjectNamespaceNamespace)
      Nothing -> ""
    fromMTtoLT t = toLazy $ fromMaybe "" t
    fromIntToInt32 = fromInteger . toInteger
    nodesToChanges :: Text -> Text -> Text -> [Maybe ProjectMergeRequestsNodesMergeRequest] -> [Change]
    nodesToChanges shortName fullName namespace nodes' =
      map toChange (catMaybes nodes')
      where
        toChange :: ProjectMergeRequestsNodesMergeRequest -> Change
        toChange mr =
          Change
            (let ID id' = Lentille.GitLab.MergeRequests.id mr in show id')
            1
            "ChangeId"
            (toLazy $ title (mr :: ProjectMergeRequestsNodesMergeRequest))
            (fromMTtoLT $ description mr)
            (fromMTtoLT $ webUrl mr)
            (fromIntToInt32 . fromMaybe 0 $ commitCount mr)
            (getAdditions $ diffStatsSummary mr)
            (getDeletions $ diffStatsSummary mr)
            (getChangedFileCount $ diffStatsSummary mr)
            (getChangedFiles $ diffStats mr)
            (getCommits $ commitsWithoutMergeCommits mr)
            (toLazy namespace)
            (toLazy fullName)
            (toLazy shortName)
            (Just $ getAuthorIdent mr)
            (Just . ChangeOptionalMergedByMergedBy $ getMergedByIdent mr)
            (toLazy $ sourceBranch mr)
            (toLazy $ targetBranch mr)
            (Just $ timeToTimestamp Nothing $ createdAt mr)
            (getMergedAt $ mergedAt mr)
            (Just $ timeToTimestamp Nothing $ updatedAt mr)
            -- No closedAt attribute for a MR ?
            Nothing
            -- For now unable to get the state https://github.com/morpheusgraphql/morpheus-graphql/issues/600
            (if isMerged $ mergedAt mr then "MERGED" else "CLOSED")
            (ChangeOptionalDurationDuration <$> getDuration mr)
            (if mergeable mr then "MERGEABLE" else "CONFLICT")
            (getLabels $ labels mr)
            (getAssignees $ assignees mr)
            (if approved mr then fromList ["APPROVED"] else fromList [])
            (draft mr)
            (ChangeOptionalSelfMergedSelfMerged <$> getSelfMerged mr)
        getDuration :: ProjectMergeRequestsNodesMergeRequest -> Maybe Int32
        getDuration ProjectMergeRequestsNodesMergeRequest {createdAt, mergedAt} = case fmap readMaybe compuDiff :: Maybe (Maybe Float) of
          Just durationFM -> truncate <$> durationFM
          _ -> Nothing
          where
            compuDiff :: Maybe String
            compuDiff = fmap (show . nominalDiffTimeToSeconds . negate . diffUTCTime (timeToUTCTime Nothing createdAt) . timeToUTCTime Nothing) mergedAt
        getMergedAt :: Maybe Time -> Maybe ChangeOptionalMergedAt
        getMergedAt tM = fmap (ChangeOptionalMergedAtMergedAt . timeToTimestamp Nothing) tM
        ghostIdent = Ident "ghost" "ghost"
        getAuthorIdent :: ProjectMergeRequestsNodesMergeRequest -> Ident
        getAuthorIdent ProjectMergeRequestsNodesMergeRequest {author} = case author of
          Just author' -> toIdent $ getUsername author'
          Nothing -> ghostIdent
          where
            getUsername ProjectMergeRequestsNodesAuthorUserCore {..} = username
        getMergedByIdent :: ProjectMergeRequestsNodesMergeRequest -> Ident
        getMergedByIdent ProjectMergeRequestsNodesMergeRequest {mergeUser} = case mergeUser of
          Just author' -> toIdent $ getUsername author'
          Nothing -> ghostIdent
          where
            getUsername ProjectMergeRequestsNodesMergeUserUserCore {..} = username
        getDiffStatsAttr :: Maybe ProjectMergeRequestsNodesDiffStatsSummaryDiffStatsSummary -> (ProjectMergeRequestsNodesDiffStatsSummaryDiffStatsSummary -> Int) -> Int32
        getDiffStatsAttr diffStatsM cb =
          case diffStatsM of
            Just diffStats -> fromIntToInt32 $ cb diffStats
            Nothing -> 0
        getAdditions diffStatsM = getDiffStatsAttr diffStatsM additions
        getDeletions diffStatsM = getDiffStatsAttr diffStatsM deletions
        getChangedFileCount diffStatsM = getDiffStatsAttr diffStatsM fileCount
        getChangedFiles :: Maybe [ProjectMergeRequestsNodesDiffStatsDiffStats] -> V.Vector ChangedFile
        getChangedFiles dfM = case dfM of
          Just df -> fromList $ map toChangeFile df
          Nothing -> fromList []
          where
            toChangeFile ProjectMergeRequestsNodesDiffStatsDiffStats {..} =
              ChangedFile
                (fromIntToInt32 additions)
                (fromIntToInt32 deletions)
                (toLazy path)
        getCommits :: Maybe ProjectMergeRequestsNodesCommitsWithoutMergeCommitsCommitConnection -> V.Vector Commit
        getCommits commitsM =
          case commitsM of
            Just
              ( ProjectMergeRequestsNodesCommitsWithoutMergeCommitsCommitConnection
                  (Just commits)
                ) -> fromList $ map toCommit (catMaybes commits)
            _ -> empty
          where
            toCommit :: ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesCommit -> Commit
            toCommit ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesCommit {..} =
              -- Unable to get the committer and the committed_at date
              -- Unable to get additions/deletions
              Commit
                (toLazy sha)
                (Just . toIdent . getUsername $ author)
                (Just . toIdent . getUsername $ author)
                (Just . timeToTimestamp formatString $ fromMaybe defaultTimestamp authoredDate)
                (Just . timeToTimestamp formatString $ fromMaybe defaultTimestamp authoredDate)
                0
                0
                (toLazy $ fromMaybe "" title)
              where
                formatString = Just "%FT%X%Ez"
            getUsername :: Maybe ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthorUserCore -> Text
            getUsername uM = case uM of
              Just u -> username (u :: ProjectMergeRequestsNodesCommitsWithoutMergeCommitsNodesAuthorUserCore)
              Nothing -> ""
            defaultTimestamp = Time "1970-01-01T00:00:00+00:00"
        toIdent :: Text -> Ident
        toIdent username' = Ident (toLazy $ "gitlab.com" <> "/" <> username') (toLazy username')
        getLabels :: Maybe ProjectMergeRequestsNodesLabelsLabelConnection -> V.Vector LText
        getLabels labelsM =
          case labelsM of
            ( Just
                ( ProjectMergeRequestsNodesLabelsLabelConnection
                    (Just labels)
                  )
              ) -> fromList $ map toLabel (catMaybes labels)
            _ -> empty
          where
            toLabel ProjectMergeRequestsNodesLabelsNodesLabel {title} = toLazy title
        getAssignees :: Maybe ProjectMergeRequestsNodesAssigneesMergeRequestAssigneeConnection -> V.Vector Ident
        getAssignees assigneesM =
          case assigneesM of
            ( Just
                ( ProjectMergeRequestsNodesAssigneesMergeRequestAssigneeConnection
                    (Just assignees)
                  )
              ) -> fromList $ map toUsername (catMaybes assignees)
            _ -> empty
          where
            toUsername ProjectMergeRequestsNodesAssigneesNodesMergeRequestAssignee {username} = toIdent username
        isMerged :: Maybe Time -> Bool
        isMerged mergedAt' = case mergedAt' of
          Just _ -> True
          Nothing -> False
        getSelfMerged :: ProjectMergeRequestsNodesMergeRequest -> Maybe Bool
        getSelfMerged ProjectMergeRequestsNodesMergeRequest {author, mergeUser, mergedAt} =
          if isMerged mergedAt then checkIsSelfMerged author mergeUser else Nothing
          where
            checkIsSelfMerged a m = case (a, m) of
              (Just (ProjectMergeRequestsNodesAuthorUserCore u1), Just (ProjectMergeRequestsNodesMergeUserUserCore u2)) -> Just $ u1 == u2
              _ -> Nothing
        timeToTimestamp :: Maybe String -> Time -> T.Timestamp
        timeToTimestamp formatStringE = T.fromUTCTime . timeToUTCTime formatStringE
        timeToUTCTime :: Maybe String -> Time -> UTCTime
        timeToUTCTime formatStringE t =
          let Time tt = t in parseTimeOrError False defaultTimeLocale (fromMaybe "%FT%XZ" formatStringE) $ toString tt
