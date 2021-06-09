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

testIndexName :: Text
testIndexName = "test-index"

withBH :: ((BH.BHEnv, BH.IndexName) -> IO ()) -> IO ()
withBH cb = bracket create cb delete
  where
    -- todo: generate random name
    create = I.createChangesIndex "http://localhost:9200" testIndexName
    delete (bhEnv, testIndex) = do
      BH.runBH bhEnv $ do
        _ <- BH.deleteIndex testIndex
        False <- BH.indexExists testIndex
        pure ()

testIndexChange :: Assertion
testIndexChange = withBH doTest
  where
    -- checkIndex bhEnv testIndex = do
    --   let query = BH.MatchAllQuery Nothing
    --   let search = BH.mkSearch (Just query) Nothing
    --   parsed <- BH.runBH bhEnv $ do
    --     resp <- BH.searchByIndex testIndex search
    --     BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.SearchResult Value))
    --   case parsed of
    --     Left _e -> error "Could not get changes back"
    --     Right sr -> assertEqual "changes" (BH.hitsTotal (BH.searchHits sr)) (BH.HitsTotal 1 BH.HTR_EQ)
    checkDocExists bhEnv index docId = do
      resp <- BH.runBH bhEnv $ do
        BH.documentExists index docId
      assertEqual "doc exist" True resp
    doCheck :: (Show a, Eq a) => (ELKChange -> a) -> a -> ELKChange -> Assertion
    doCheck field value change = assertEqual "change field match" (field change) value
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

    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      I.indexChanges bhEnv testIndex [fakeChange1, fakeChange2]
      checkDocExists bhEnv testIndex (I.getChangeDocId fakeChange1)
      checkELKChangeField
        bhEnv
        testIndex
        (I.getChangeDocId fakeChange1)
        elkchangeTitle
        (toText (elkchangeTitle fakeChange1))
      checkDocExists bhEnv testIndex (I.getChangeDocId fakeChange2)
      checkELKChangeField
        bhEnv
        testIndex
        (I.getChangeDocId fakeChange2)
        elkchangeTitle
        (toText (elkchangeTitle fakeChange2))
    fakeChange1 = mkFakeChange 1 "My change 1"
    fakeChange2 = mkFakeChange 2 "My change 2"
