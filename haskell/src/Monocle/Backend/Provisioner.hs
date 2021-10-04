{-# LANGUAGE RecordWildCards #-}

-- | A test module to load some fake data
module Monocle.Backend.Provisioner where

import Data.Time.Clock.System
import qualified Faker
import qualified Faker.Combinators
import qualified Faker.DateTime
import qualified Faker.Movie.BackToTheFuture
import qualified Faker.TvShow.Futurama
import Monocle.Api.Config (defaultTenant)
import Monocle.Backend.Documents
import qualified Monocle.Backend.Test as T
import Monocle.Env (testTenantM)
import Monocle.Prelude

-- | Provision fakedata for a tenant
runProvisioner :: Text -> IO ()
runProvisioner tenantName = do
  events <- createFakeEvents
  putTextLn $ "[provisioner] Adding " <> show (length events) <> " events to " <> tenantName <> "."
  testTenantM (defaultTenant tenantName) $ T.indexScenario events
  putTextLn $ "[provisioner] Done."

-- | Ensure changes have a unique ID
setChangeID :: [ELKChange] -> IO [ELKChange]
setChangeID xs = do
  -- create ids using epoch as a prefix
  MkSystemTime sec _ <- getSystemTime
  let mkid x = show sec <> show x
  let newChanges = zipWith (\c x -> c {elkchangeId = mkid x}) xs ([0 ..] :: [Int])
  pure $ map (\c -> c {elkchangeUrl = "http://review.example.org/" <> elkchangeId c}) newChanges

-- | Creates a bunch of event in the last 3 weeks
createFakeEvents :: IO [T.ScenarioEvent]
createFakeEvents = do
  now <- getCurrentTime
  from <- pure $ addUTCTime (-3600 * 24 * 7 * 3) now
  baseChanges <- Faker.generateNonDeterministic $ Faker.Combinators.listOf 10 $ fakeChange from now
  changes <- setChangeID baseChanges
  pure $ T.SChange <$> changes

fakeFileCount :: Faker.Fake Word32
fakeFileCount = Faker.Combinators.fromRange (0, 42)

fakeCommitCount :: Faker.Fake Word32
fakeCommitCount = Faker.Combinators.fromRange (1, 6)

fakeTitle :: Faker.Fake LText
fakeTitle = toLazy <$> Faker.Movie.BackToTheFuture.quotes

fakeAuthor :: Faker.Fake Author
fakeAuthor = do
  name <- toLazy <$> Faker.TvShow.Futurama.characters
  pure $ Author name name

fakeText :: Faker.Fake LText
fakeText = toLazy <$> Faker.TvShow.Futurama.quotes

fakeChange :: UTCTime -> UTCTime -> Faker.Fake ELKChange
fakeChange from to = do
  elkchangeId <- pure $ ""
  elkchangeType <- pure $ EChangeDoc
  elkchangeNumber <- pure $ 1
  elkchangeChangeId <- pure $ "change-id"
  elkchangeTitle <- fakeTitle
  elkchangeUrl <- pure $ ""
  elkchangeCommitCount <- fakeCommitCount
  elkchangeAdditions <- fakeFileCount
  elkchangeDeletions <- fakeFileCount
  elkchangeChangedFilesCount <- fakeFileCount
  elkchangeChangedFiles <- pure $ [File 0 0 "/fake/path"]
  elkchangeText <- fakeText
  elkchangeCommits <- pure $ []
  elkchangeRepositoryPrefix <- pure $ ""
  elkchangeRepositoryFullname <- pure $ ""
  elkchangeRepositoryShortname <- pure $ ""
  elkchangeAuthor <- fakeAuthor
  elkchangeBranch <- pure $ ""
  elkchangeCreatedAt <- dropTime <$> Faker.DateTime.utcBetween from to
  elkchangeUpdatedAt <- dropTime <$> Faker.DateTime.utcBetween elkchangeCreatedAt to
  elkchangeMergedBy <- pure $ Nothing
  elkchangeTargetBranch <- pure $ "main"
  elkchangeMergedAt <- pure $ Nothing
  elkchangeClosedAt <- pure $ Nothing
  elkchangeDuration <- pure $ Nothing
  elkchangeApproval <- pure $ Just ["OK"]
  elkchangeSelfMerged <- pure $ Nothing
  elkchangeTasksData <- pure $ Nothing
  elkchangeState <- pure $ EChangeOpen
  elkchangeMergeable <- Faker.Combinators.frequency [(5, pure "MERGEABLE"), (1, pure "")]
  elkchangeLabels <- pure $ []
  elkchangeAssignees <- pure $ []
  elkchangeDraft <- pure $ False
  pure $ ELKChange {..}
