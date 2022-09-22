{-# LANGUAGE RecordWildCards #-}

-- | A test module to load some fake data
module Monocle.Backend.Provisioner (
  -- * Provisioner
  runProvisioner,

  -- * Fake datas
  fakeAuthor,
  fakeChange,
  fakeChangeEvent,
  fakeETaskData,
  fakeTaskData,

  -- * re-exports
  Faker.generateNonDeterministic,
) where

import Data.Time.Clock.System
import Faker qualified
import Faker.Combinators qualified
import Faker.Creature.Dog qualified
import Faker.DateTime qualified
import Faker.Movie.BackToTheFuture qualified
import Faker.TvShow.Futurama qualified
import Faker.TvShow.TheExpanse qualified
import Google.Protobuf.Timestamp qualified (fromUTCTime)
import Monocle.Backend.Documents
import Monocle.Backend.Index qualified as I
import Monocle.Backend.Test qualified as T
import Monocle.Config (mkTenant)
import Monocle.Env (testQueryM)
import Monocle.Prelude
import Monocle.Protob.Search (TaskData (..))

-- | Provision fakedata for a tenant
runProvisioner :: Text -> IO ()
runProvisioner tenantName = undefined
{-
  testQueryM' @es (mkTenant tenantName) do
  I.ensureIndex
  events <- liftIO createFakeEvents
  logInfo'  ("[provisioner] Adding " <> show (length events) <> " events to " <> tenantName <> ".") []
  T.indexScenario events
  logInfo' "[provisioner] Done." []
-}

-- | Ensure changes have a unique ID
setChangeID :: [EChange] -> IO [EChange]
setChangeID xs = do
  -- create ids using epoch as a prefix
  MkSystemTime sec _ <- getSystemTime
  let mkid x = show sec <> show x
  let newChanges = zipWith (\c x -> c {echangeId = mkid x}) xs ([0 ..] :: [Int])
  pure $
    map
      ( \c ->
          c
            { echangeChangeId = "change-" <> echangeId c
            , echangeUrl = "http://review.example.org/" <> echangeId c
            }
      )
      newChanges

-- | Creates a bunch of event in the last 3 weeks
createFakeEvents :: IO [T.ScenarioEvent]
createFakeEvents = do
  now <- getCurrentTime
  let from' = addUTCTime (-3600 * 24 * 7 * 3) now
  baseChanges <- Faker.generateNonDeterministic $ Faker.Combinators.listOf 10 $ fakeChange from' now
  changes <- setChangeID baseChanges
  pure $ T.SChange <$> changes

fakeUrl :: Text -> Faker.Fake Text
fakeUrl uid = pure $ "http://example.org/" <> uid

fakeFileCount :: Faker.Fake Word32
fakeFileCount = Faker.Combinators.fromRange (0, 42)

fakeCommitCount :: Faker.Fake Word32
fakeCommitCount = Faker.Combinators.fromRange (1, 6)

fakeTitle :: Faker.Fake LText
fakeTitle = from <$> Faker.Movie.BackToTheFuture.quotes

fakeAuthor :: Faker.Fake Author
fakeAuthor = do
  name <- from <$> Faker.TvShow.Futurama.characters
  pure $ Author name name

fakeText :: Faker.Fake LText
fakeText = from <$> Faker.TvShow.Futurama.quotes

fakeChange :: UTCTime -> UTCTime -> Faker.Fake EChange
fakeChange from' to = do
  let echangeId = ""
  let echangeType = EChangeDoc
  let echangeNumber = 1
  let echangeChangeId = "change-id"
  echangeTitle <- fakeTitle
  let echangeUrl = ""
  echangeCommitCount <- fakeCommitCount
  echangeAdditions <- fakeFileCount
  echangeDeletions <- fakeFileCount
  echangeChangedFilesCount <- fakeFileCount
  let echangeChangedFiles = [File 0 0 "/fake/path"]
  echangeText <- fakeText
  let echangeCommits = []
  let echangeRepositoryPrefix = ""
  let echangeRepositoryFullname = ""
  let echangeRepositoryShortname = ""
  echangeAuthor <- fakeAuthor
  let echangeBranch = ""
  echangeCreatedAt <- dropTime <$> Faker.DateTime.utcBetween from' to
  echangeUpdatedAt <- dropTime <$> Faker.DateTime.utcBetween echangeCreatedAt to
  let echangeMergedBy = Nothing
  let echangeTargetBranch = "main"
  let echangeMergedAt = Nothing
  let echangeClosedAt = Nothing
  let echangeDuration = Nothing
  let echangeApproval = Just ["OK"]
  let echangeSelfMerged = Nothing
  let echangeTasksData = Nothing
  let echangeState = EChangeOpen
  echangeMergeable <- Faker.Combinators.frequency [(5, pure "MERGEABLE"), (1, pure "")]
  let echangeLabels = []
  let echangeAssignees = []
  let echangeDraft = False
  pure $ EChange {..}

fakeChangeEvent :: UTCTime -> UTCTime -> Faker.Fake EChangeEvent
fakeChangeEvent from' to = do
  echangeeventType <- Faker.Combinators.elements [minBound .. maxBound]
  echangeeventOnAuthor <- fakeAuthor
  echangeeventCreatedAt <- dropTime <$> Faker.DateTime.utcBetween from' to
  echangeeventOnCreatedAt <- dropTime <$> Faker.DateTime.utcBetween echangeeventCreatedAt to
  let echangeeventId = ""
      echangeeventNumber = 0
      echangeeventChangeId = ""
      echangeeventUrl = ""
      echangeeventChangedFiles = []
      echangeeventRepositoryPrefix = ""
      echangeeventRepositoryShortname = ""
      echangeeventRepositoryFullname = ""
      echangeeventAuthor = Nothing
      echangeeventSelfMerged = Nothing
      echangeeventBranch = ""
      echangeeventApproval = Nothing
      echangeeventTasksData = Nothing
      echangeeventLabels = Just []
      echangeeventDuration = Nothing
  pure $ EChangeEvent {..}

fakeTaskId :: Faker.Fake Text
fakeTaskId = show @Text @Int <$> Faker.Combinators.fromRange (1, 65535)

fakeTaskPrefix :: Faker.Fake Text
fakeTaskPrefix = Faker.Combinators.elements ["rhbz#", "gh#", "lada"]

fakeETaskData :: Faker.Fake ETaskData
fakeETaskData = do
  tdTid <- fakeTaskId
  tdCrawlerName <- Just <$> Faker.Creature.Dog.name
  tdTtype <- (: []) <$> Faker.Creature.Dog.sound
  tdUpdatedAt <- toMonocleTime <$> Faker.DateTime.utc
  let tdChangeUrl = "no-change"
  tdSeverity <- Faker.TvShow.TheExpanse.locations
  tdPriority <- Faker.TvShow.TheExpanse.ships
  tdScore <- Faker.Combinators.fromRange (0, 42)
  tdUrl <- fakeUrl tdTid
  tdTitle <- Faker.TvShow.TheExpanse.quotes
  tdPrefix <- Just <$> fakeTaskPrefix
  pure $ ETaskData {..}

fakeTaskData :: Faker.Fake TaskData
fakeTaskData = do
  taskDataUpdatedAt <- Just . Google.Protobuf.Timestamp.fromUTCTime <$> Faker.DateTime.utc
  let taskDataChangeUrl = "no-change"
  taskDataTtype <- fromList . (: []) . from <$> Faker.Creature.Dog.sound
  taskDataTid <- from <$> fakeTaskId
  taskDataUrl <- from <$> fakeUrl (from taskDataTid)
  taskDataTitle <- from <$> Faker.TvShow.TheExpanse.quotes
  taskDataSeverity <- from <$> Faker.TvShow.TheExpanse.locations
  taskDataPriority <- from <$> Faker.TvShow.TheExpanse.ships
  taskDataScore <- Faker.Combinators.fromRange (0, 42)
  taskDataPrefix <- from <$> fakeTaskPrefix
  pure $ TaskData {..}
