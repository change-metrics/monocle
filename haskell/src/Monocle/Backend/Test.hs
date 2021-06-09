{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Test where

import Control.Exception (bracket)
import Data.Time.Format
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Google.Protobuf.Timestamp as T
import Monocle.Backend.Documents
import qualified Monocle.Backend.Index as I
import Monocle.Change
import Relude
import Test.Tasty.HUnit

fakeDate :: T.Timestamp
fakeDate = T.fromUTCTime $ parseTimeOrError False defaultTimeLocale "%F" "2021-01-01"

fakeIdent :: Ident
fakeIdent = Ident "John" "John"

mkFakeChange :: Int32 -> LText -> Change
mkFakeChange number title =
  fakeChange
    { changeId = "aFakeId-" <> show number,
      changeNumber = number,
      changeTitle = title,
      changeUrl = "https://fakehost/change/" <> show number
    }

fakeChange :: Change
fakeChange =
  Change
    { changeId = "",
      changeNumber = 1,
      changeChangeId = "change-id",
      changeTitle = "",
      changeUrl = "",
      changeCommitCount = 1,
      changeAdditions = 1,
      changeDeletions = 1,
      changeChangedFilesCount = 1,
      changeChangedFiles = V.fromList [ChangedFile 0 0 "/fake/path"],
      changeText = "",
      changeCommits = V.fromList [],
      changeRepositoryPrefix = "",
      changeRepositoryFullname = "",
      changeRepositoryShortname = "",
      changeAuthor = Just fakeIdent,
      changeOptionalMergedBy = Nothing,
      changeBranch = "",
      changeTargetBranch = "",
      changeCreatedAt = Just fakeDate,
      changeOptionalMergedAt = Nothing,
      changeUpdatedAt = Just fakeDate,
      changeOptionalClosedAt = Nothing,
      changeState = "OPEN",
      changeOptionalDuration = Nothing,
      changeMergeable = "",
      changeLabels = V.fromList [],
      changeAssignees = V.fromList [],
      changeApprovals = V.fromList [],
      changeDraft = False,
      changeOptionalSelfMerged = Nothing
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
    checkELKChangeField :: BH.BHEnv -> BH.IndexName -> BH.DocId -> (ELKChange -> Bool) -> IO ()
    checkELKChangeField bhEnv index docId assertF = do
      parsed <- BH.runBH bhEnv $ do
        resp <- BH.getDocument index docId
        BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.EsResult ELKChange))
      case parsed of
        Left _e -> error "Could not get changes back"
        Right sr -> assertEqual "changes" (checkHit $ BH.foundResult sr) True
      where
        checkHit :: Maybe (BH.EsResultFound ELKChange) -> Bool
        checkHit (Just (BH.EsResultFound _ change)) = assertF change
        checkHit _ = False

    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      I.indexChanges bhEnv testIndex [fakeChange1, fakeChange2]
      checkDocExists bhEnv testIndex (I.getChangeDocId fakeChange1)
      checkELKChangeField
        bhEnv
        testIndex
        (I.getChangeDocId fakeChange1)
        (\c -> elkchangeTitle c == toText (changeTitle fakeChange1))
      checkDocExists bhEnv testIndex (I.getChangeDocId fakeChange2)
      checkELKChangeField
        bhEnv
        testIndex
        (I.getChangeDocId fakeChange2)
        (\c -> elkchangeTitle c == toText (changeTitle fakeChange2))
    fakeChange1 = mkFakeChange 1 "My change 1"
    fakeChange2 = mkFakeChange 2 "My change 2"
