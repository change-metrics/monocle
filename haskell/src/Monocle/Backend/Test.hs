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

checkDocExists :: BH.BHEnv -> BH.IndexName -> BH.DocId -> IO ()
checkDocExists bhEnv index docId = do
  resp <- BH.runBH bhEnv $ do
    BH.documentExists index docId
  assertEqual "doc exist" True resp

checkELKChangeField :: (Show a, Eq a) => BH.BHEnv -> BH.IndexName -> BH.DocId -> (ELKChange -> a) -> a -> IO ()
checkELKChangeField bhEnv index docId field value = do
  parsed <- BH.runBH bhEnv $ do
    resp <- BH.getDocument index docId
    BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.EsResult ELKChange))
  case parsed of
    Left _e -> error "Could not get changes back"
    Right sr -> checkHit $ BH.foundResult sr
  where
    checkHit :: Maybe (BH.EsResultFound ELKChange) -> Assertion
    checkHit (Just (BH.EsResultFound _ change)) = doCheck field value change
    checkHit Nothing = error "Change not found"
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

-- checkChangesCount' :: BH.BHEnv -> BH.IndexName -> Int -> IO ()
-- checkChangesCount' bhEnv index expectedCount = do
--   parsed <- BH.runBH bhEnv $ do
--     resp <-
--       BH.searchByIndex
--         index
--         ( BH.mkSearch (Just (BH.TermQuery (BH.Term "type" "Change") Nothing)) Nothing
--         )
--     BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.SearchResult ELKChange))
--   case parsed of
--     Left _ -> error "Couldn't count changes"
--     Right sr -> assertEqual "changes" (BH.HitsTotal expectedCount BH.HTR_EQ) (BH.hitsTotal (BH.searchHits sr))

testIndexChanges :: Assertion
testIndexChanges = withBH doTest
  where
    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      -- Index two Changes and check present in database
      indexChanges [fakeChange1, fakeChange2]
      checkDocExists bhEnv testIndex (I.getChangeDocId fakeChange1)
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
        checkDocExists' = checkDocExists bhEnv testIndex
        checkELKChangeField' :: (Show a, Eq a) => BH.DocId -> (ELKChange -> a) -> a -> IO ()
        checkELKChangeField' = checkELKChangeField bhEnv testIndex
        checkChangeCount' = checkChangesCount bhEnv testIndex
        fakeChange1 = mkFakeChange 1 "My change 1"
        fakeChange1Updated = fakeChange1 {elkchangeTitle = "My change 1 updated"}
        fakeChange2 = mkFakeChange 2 "My change 2"
