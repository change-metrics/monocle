{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Test where

import Control.Exception (bracket)
import Data.Time.Clock (UTCTime)
import qualified Database.Bloodhound as BH
import Monocle.Backend.Documents
import qualified Monocle.Backend.Index as I
import Relude
import Test.Tasty.HUnit

fakeDate :: UTCTime
fakeDate = fromMaybe (error "nop") (readMaybe "2021-05-31 10:00:00 Z")

fakeDateAlt :: UTCTime
fakeDateAlt = fromMaybe (error "nop") (readMaybe "2021-06-01 20:00:00 Z")

fakeAuthor :: Author
fakeAuthor = Author "John"

mkFakeChange :: Int -> Text -> ELKChange
mkFakeChange number title =
  fakeChange
    { elkchangeId = "aFakeId-" <> show number,
      elkchangeNumber = number,
      elkchangeTitle = title,
      elkchangeUrl = "https://fakehost/change/" <> show number
    }

fakeChange :: ELKChange
fakeChange =
  ELKChange
    { elkchangeId = "",
      elkchangeType = "Change",
      elkchangeNumber = 1,
      elkchangeChangeId = "change-id",
      elkchangeTitle = "",
      elkchangeUrl = "",
      elkchangeCommitCount = 1,
      elkchangeAdditions = 1,
      elkchangeDeletions = 1,
      elkchangeChangedFilesCount = 1,
      elkchangeChangedFiles = [File 0 0 "/fake/path"],
      elkchangeText = "",
      elkchangeCommits = [],
      elkchangeRepositoryPrefix = "",
      elkchangeRepositoryFullname = "",
      elkchangeRepositoryShortname = "",
      elkchangeAuthor = fakeAuthor,
      elkchangeBranch = "",
      elkchangeCreatedAt = fakeDate,
      elkchangeUpdatedAt = fakeDate,
      elkchangeCommitter = Nothing,
      elkchangeMergedBy = Nothing,
      elkchangeTargetBranch = "main",
      elkchangeMergedAt = Nothing,
      elkchangeClosedAt = Nothing,
      elkchangeDuration = Nothing,
      elkchangeApproval = Just ["OK"],
      elkchangeSelfMerged = Nothing,
      elkchangeTasksData = Nothing,
      elkchangeState = "OPEN",
      elkchangeMergeable = "",
      elkchangeLabels = [],
      elkchangeAssignees = [],
      elkchangeDraft = False
    }

fakeELKCrawlerMetadata :: ELKCrawlerMetadata
fakeELKCrawlerMetadata = ELKCrawlerMetadata fakeDate

testIndexName :: BH.IndexName
testIndexName = BH.IndexName "test-index"

withBH :: ((BH.BHEnv, BH.IndexName) -> IO ()) -> IO ()
withBH = bracket create delete
  where
    -- todo: generate random name
    create = I.createChangesIndex "http://localhost:9200" testIndexName
    delete (bhEnv, testIndex) = do
      BH.runBH bhEnv $ do
        resp <- BH.deleteIndex testIndex
        print resp
        False <- BH.indexExists testIndex
        pure ()

checkELKChangeField :: (Show a, Eq a) => BH.BHEnv -> BH.IndexName -> BH.DocId -> (ELKChange -> a) -> a -> IO ()
checkELKChangeField bhEnv index docId field value = do
  docM <- I.getDocument bhEnv index docId :: IO (Maybe ELKChange)
  case docM of
    Just change -> doCheck field value change
    Nothing -> error "Change not found"
  where
    doCheck :: (Show a, Eq a) => (ELKChange -> a) -> a -> ELKChange -> Assertion
    doCheck field' value' change = assertEqual "change field match" (field' change) value'

checkChangesCount :: BH.BHEnv -> BH.IndexName -> Int -> IO ()
checkChangesCount bhEnv index expectedCount = do
  resp <- BH.runBH bhEnv $ do
    BH.countByIndex
      index
      ( BH.CountQuery (BH.TermQuery (BH.Term "type" "Change") Nothing)
      )
  case resp of
    Left _ -> error "Couldn't count changes"
    Right countD -> assertEqual "check change count" expectedCount (fromEnum $ BH.crCount countD)

testIndexChanges :: Assertion
testIndexChanges = withBH doTest
  where
    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      -- Index two Changes and check present in database
      indexChanges [fakeChange1, fakeChange2]
      checkDocExists' $ I.getChangeDocId fakeChange1
      checkELKChangeField'
        (I.getChangeDocId fakeChange1)
        elkchangeTitle
        (toText (elkchangeTitle fakeChange1))
      checkDocExists' $ I.getChangeDocId fakeChange2
      checkELKChangeField'
        (I.getChangeDocId fakeChange2)
        elkchangeTitle
        (toText (elkchangeTitle fakeChange2))
      -- Update a Change and ensure the document is updated in the database
      indexChanges [fakeChange1Updated]
      checkDocExists' $ I.getChangeDocId fakeChange1
      checkELKChangeField'
        (I.getChangeDocId fakeChange1Updated)
        elkchangeTitle
        (toText (elkchangeTitle fakeChange1Updated))
      -- Check total count of Change document in the database
      checkChangeCount' 2
      where
        indexChanges = I.indexChanges bhEnv testIndex
        checkDocExists' dId = do
          exists <- I.checkDocExists bhEnv testIndex dId
          assertEqual "check doc exists" exists True
        checkELKChangeField' :: (Show a, Eq a) => BH.DocId -> (ELKChange -> a) -> a -> IO ()
        checkELKChangeField' = checkELKChangeField bhEnv testIndex
        checkChangeCount' = checkChangesCount bhEnv testIndex
        fakeChange1 = mkFakeChange 1 "My change 1"
        fakeChange1Updated = fakeChange1 {elkchangeTitle = "My change 1 updated"}
        fakeChange2 = mkFakeChange 2 "My change 2"

testCrawlerMetadata :: Assertion
testCrawlerMetadata = withBH doTest
  where
    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      -- No previous
      defaultLastUpdatedDate <- I.getLastUpdated bhEnv testIndex entity
      assertEqual "check default date" defaultLastUpdatedDate expectedDefaultDate
      -- Set inital last updated date
      I.setLastUpdated bhEnv testIndex entity fakeDate
      lastUpdated <- I.getLastUpdated bhEnv testIndex entity
      assertEqual "check date similar" lastUpdated fakeDate
      -- Set a new last updated date
      I.setLastUpdated bhEnv testIndex entity fakeDateAlt
      lastUpdated' <- I.getLastUpdated bhEnv testIndex entity
      assertEqual "check date similar" lastUpdated' fakeDateAlt
      where
        entity = I.Project "nova"
        expectedDefaultDate :: UTCTime
        expectedDefaultDate = fromMaybe (error "nop") (readMaybe "2021-01-01 00:00:00 UTC")
