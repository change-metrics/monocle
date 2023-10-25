{-# OPTIONS_GHC -Wno-orphans #-}

module Lentille.GitHub.Utils where

import Data.Morpheus.Client (ID (ID))
import Google.Protobuf.Timestamp qualified as T
import Lentille (isClosed, isMerged, swapDuration)
import Lentille.GitHub.RateLimit (getRateLimit, retryCheck)
import Lentille.GitHub.Types
import Lentille.GraphQL (GraphClient, GraphEffects, RateLimit (..), StreamFetchOptParams (..), host)
import Monocle.Prelude
import Monocle.Protob.Change
import Proto3.Suite (Enumerated (Enumerated))

instance From DateTime UTCTime where
  from = dateTimeToUTCTime

getURL :: URI -> Text
getURL (URI url) = url

getSHA :: GitObjectID -> Text
getSHA (GitObjectID sha) = sha

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

toApprovals :: PullRequestReviewDecision -> Text
toApprovals = \case
  PullRequestReviewDecisionAPPROVED -> "APPROVED"
  PullRequestReviewDecisionCHANGES_REQUESTED -> "CHANGES_REQUESTED"
  PullRequestReviewDecisionREVIEW_REQUIRED -> "REVIEW_REQUIRED"

toPRMergeableState :: MergeableState -> Text
toPRMergeableState = \case
  MergeableStateCONFLICTING -> "CONFLICT"
  MergeableStateMERGEABLE -> "MERGEABLE"
  MergeableStateUNKNOWN -> "UNKNOWN"

toPRState :: PullRequestState -> Enumerated Change_ChangeState
toPRState = \case
  PullRequestStateCLOSED -> Enumerated (Right Change_ChangeStateClosed)
  PullRequestStateMERGED -> Enumerated $ Right Change_ChangeStateMerged
  PullRequestStateOPEN -> Enumerated $ Right Change_ChangeStateOpen

toApproval :: PullRequestReviewState -> Text
toApproval = \case
  PullRequestReviewStateAPPROVED -> "APPROVED"
  PullRequestReviewStateCHANGES_REQUESTED -> "CHANGES_REQUESTED"
  PullRequestReviewStateCOMMENTED -> "COMMENTED"
  PullRequestReviewStateDISMISSED -> "DISMISSED"
  PullRequestReviewStatePENDING -> "PENDING"

toDuration :: DateTime -> DateTime -> ChangeOptionalDuration
toDuration d1 d2 = ChangeOptionalDurationDuration . from $ diffTimeSec (from d1) (from d2)

toChangeFilePath :: ChangedFile -> ChangedFilePath
toChangeFilePath (ChangedFile _ _ path) = ChangedFilePath path

toChangeEventMergedCommitSha :: ChangeOptionalMergedCommitSha -> ChangeEventOptionalMergedCommitSha
toChangeEventMergedCommitSha (ChangeOptionalMergedCommitShaMergedCommitSha sha) = ChangeEventOptionalMergedCommitShaMergedCommitSha sha

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
    , changeEventDraft = changeDraft change
    , -- To be filled by caller function
      changeEventCreatedAt = Nothing
    , changeEventAuthor = Nothing
    , changeEventOptionalDuration = swapDuration <$> changeOptionalDuration change
    , changeEventOptionalMergedCommitSha = toChangeEventMergedCommitSha <$> changeOptionalMergedCommitSha change
    }

instance From DateTime ChangeOptionalClosedAt where
  from = ChangeOptionalClosedAtClosedAt . dateTimeToTimestamp

instance From DateTime ChangeOptionalMergedAt where
  from = ChangeOptionalMergedAtMergedAt . dateTimeToTimestamp

instance From DateTime T.Timestamp where
  from = dateTimeToTimestamp

optParams ::
  GraphEffects es => StreamFetchOptParams es a
optParams =
  let fpRetryCheck = retryCheck
      fpDepth = Just 25
      fpGetRatelimit = Just getRateLimit
   in StreamFetchOptParams {..}

getID :: ID -> Text
getID (ID v) = v

toMaybeMergedOrAbandonedEvent :: Change -> ID -> (Text -> Ident) -> Text -> DateTime -> Maybe ChangeEvent
toMaybeMergedOrAbandonedEvent change eId getIdent actor createdAt =
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

toMaybeReviewEvent :: Change -> ID -> (Text -> Ident) -> Text -> DateTime -> PullRequestReviewState -> Maybe ChangeEvent
toMaybeReviewEvent change eId getIdent actor createdAt reviewState =
  Just
    $ ( baseEvent
          (ChangeEventTypeChangeReviewed $ ChangeReviewedEvent $ fromList [from $ toApproval reviewState])
          (from $ getID eId)
          change
      )
      { changeEventAuthor = Just $ getIdent actor
      , changeEventCreatedAt = Just $ from createdAt
      }

toMaybeForcePushedEvent :: Change -> ID -> (Text -> Ident) -> Text -> DateTime -> Maybe ChangeEvent
toMaybeForcePushedEvent change eId getIdent actor createdAt =
  Just
    $ ( baseEvent
          (ChangeEventTypeChangeCommitForcePushed ChangeCommitForcePushedEvent)
          (from $ getID eId)
          change
      )
      { changeEventAuthor = Just $ getIdent actor
      , changeEventCreatedAt = Just $ from createdAt
      }

toMaybeCommentEvent :: Change -> ID -> (Text -> Ident) -> Text -> DateTime -> Maybe ChangeEvent
toMaybeCommentEvent change eId getIdent actor createdAt =
  Just
    $ ( baseEvent
          (ChangeEventTypeChangeCommented ChangeCommentedEvent)
          (from $ getID eId)
          change
      )
      { changeEventAuthor = Just $ getIdent actor
      , changeEventCreatedAt = Just $ from createdAt
      }

createdEvent :: Change -> ChangeEvent
createdEvent change =
  ( baseEvent
      (ChangeEventTypeChangeCreated ChangeCreatedEvent)
      ("CCE" <> changeId change)
      change
  )
    { changeEventCreatedAt = changeCreatedAt change
    , changeEventAuthor = changeAuthor change
    }

toChangeOptionalDuration :: PullRequestState -> DateTime -> Maybe DateTime -> DateTime -> Maybe ChangeOptionalDuration
toChangeOptionalDuration s updatedAt closedAt createdAt =
  if isMerged (toPRState s) || isClosed (toPRState s)
    then Just $ toDuration (fromMaybe updatedAt closedAt) createdAt
    else Nothing

extractRateLimit :: Int -> Int -> Text -> RateLimit
extractRateLimit used remaining resetAtText = case parseDateValue $ from resetAtText of
  Just resetAt -> RateLimit {..}
  Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText

getHost :: GraphClient -> Text
getHost client =
  let host' = host client
   in if host' == "api.github.com" then "github.com" else host'
