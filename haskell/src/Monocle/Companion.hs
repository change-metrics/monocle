-- |
-- This module contains logic for the companion feature
module Monocle.Companion where

import Monocle.Api.Config (defaultTenant)
import Monocle.Env
import Monocle.Prelude
import Monocle.Search.Syntax (Expr (..))

-- $setup
-- >>> let mkDate s = (fromMaybe (error "oops") $ readMaybe s) :: UTCTime

-- | A project definition name
newtype Target = Target Text

-- | A duration
newtype ReviewTime = ReviewTime Natural

data Frequency = Daily | Weekly

-- | A quantity
newtype ReviewCount = ReviewCount Natural
  deriving newtype (Num)

-- | Companion goal
data Goal
  = -- | The maintainer goal is achieved by reviewing new changes on time
    Maintainer ReviewTime
  | -- | The contributor goal is achieved by reviewing changes periodically
    Contributor UTCTime ReviewCount Frequency

-- | How to contact participant, e.g. a mail address
newtype ContactInfo = ContactInfo Text
  deriving (Eq, Show)

-- | The companion output
data Advice = Advice
  { adviceTitle :: Text,
    adviceBody :: Text,
    adviceReceipient :: ContactInfo
  }
  deriving (Eq, Show)

-- | Companion participant info
data Participant = Participant
  { participantAccount :: Text,
    participantContact :: ContactInfo
  }

-- | Companion main function, returns the time it needs to run again, and an advice if any.
checkGoal :: Participant -> Target -> Goal -> TenantM (UTCTime, Maybe Advice)
checkGoal Participant {..} (Target project) goal = do
  now <- liftIO getCurrentTime
  -- start with a query scoped to the project
  query <- mkQuery participantAccount (Just (EqExpr "project" project))
  runQueryM query $ case goal of
    Maintainer (ReviewTime timeLimit) -> do
      let nextCheck = timeLimit - timeLimit `div` 4
      -- TODO: create advice for new changes getting too old
      _changesWithoutReview <- pure []
      pure (addNatToTime nextCheck now, Nothing)
    Contributor since (ReviewCount _minReview) freq -> do
      let (_periodCount, periodStart, periodPos) = countPeriod freq since now
          nextCheck = getNextCheckDate periodPos freq periodStart
      -- TODO: when reviewCount < minReview, look for change to review
      pure (nextCheck, Nothing)

getNextCheckDate :: Natural -> Frequency -> UTCTime -> UTCTime
getNextCheckDate periodPos freq = addNatToTime duration
  where
    duration =
      if
          -- Current date is too early, wait until mid-term
          | periodPos < 4 -> periodLength `div` 2
          -- currently mid-term, recheck at 75% of the term
          | periodPos < 8 -> periodLength - periodLength `div` 4
          -- end of the term, wait until next mid-term
          | otherwise -> periodLength + periodLength `div` 2
    periodLength = freqToSec freq

testParticipant :: Participant
testParticipant = Participant "alice" (ContactInfo "@alice:matrix.org")

testProject :: Target
testProject = Target "nova"

testContributorGoal :: Assertion
testContributorGoal = testTenantM config $ do
  now <- liftIO getCurrentTime
  (_next, adviceM) <- checkGoal testParticipant testProject (goal now)
  liftIO $ assertEqual "no advice" Nothing adviceM
  where
    config = defaultTenant
    goal now = Contributor now 5 Daily

-- | Calculate the total period count and the current period position (from 0 to 10).
--
-- >>> countPeriod Daily (mkDate "2000-01-01 00:00:00 Z") (mkDate "2000-01-01 12:00:00 Z")
-- (0,2000-01-01 00:00:00 UTC,5)
--
-- countPeriod Daily (mkDate "2000-01-01 00:00:00 Z") (mkDate "2000-01-01 23:50:00 Z")
-- (0,2000-01-01 00:00:00 UTC,9)
--
-- >>> countPeriod Weekly (mkDate "2000-01-01 00:00:00 Z") (mkDate "2000-01-08 01:00:00 Z")
-- (1,2000-01-08 00:00:00 UTC,0)
countPeriod :: Frequency -> UTCTime -> UTCTime -> (Natural, UTCTime, Natural)
countPeriod freq since now = (periodCount, periodStart, fromInteger $ toInteger $ periodPos)
  where
    periodStart = addNatToTime (periodCount * periodSeconds) since
    seconds = round $ elapsedSeconds since now
    periodCount = seconds `div` periodSeconds
    periodPos = (seconds `mod` periodSeconds) * 10 `div` periodSeconds
    periodSeconds = freqToSec freq

addNatToTime :: Natural -> UTCTime -> UTCTime
addNatToTime nat = addUTCTime (fromInteger $ toInteger nat)

freqToSec :: Frequency -> Natural
freqToSec = \case
  Daily -> 24 * 3600
  Weekly -> 7 * 24 * 3600
