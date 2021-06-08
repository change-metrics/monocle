{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Test where

import Control.Exception (bracket)
import Data.Aeson (Value)
import Data.Time.Format
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Google.Protobuf.Timestamp as T
import qualified Monocle.Backend.Index as C
import Monocle.Change
import qualified Monocle.Search.Queries as Q
import Relude
import Test.Tasty.HUnit

fakeDate :: T.Timestamp
fakeDate = T.fromUTCTime $ parseTimeOrError False defaultTimeLocale "%F" "2021-01-01"

fakeIdent :: Ident
fakeIdent = Ident "John" "John"

fakeChange :: Change
fakeChange =
  Change
    { changeId = "aFakeId",
      changeNumber = 1,
      changeChangeId = "change-id",
      changeTitle = "change-title",
      changeUrl = "https://url",
      changeCommitCount = 1,
      changeAdditions = 1,
      changeDeletions = 1,
      changeChangedFilesCount = 1,
      changeChangedFiles = V.fromList [ChangedFile 0 0 "/fake/path"],
      changeText = "",
      changeCommits =
        V.fromList
          [ Commit
              { commitSha = "",
                commitAuthor = Just fakeIdent,
                commitCommitter = Just fakeIdent,
                commitAuthoredAt = Just fakeDate,
                commitCommittedAt = Just fakeDate,
                commitAdditions = 0,
                commitDeletions = 0,
                commitTitle = ""
              }
          ],
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

withBH :: ((BH.BHEnv, BH.IndexName) -> IO ()) -> IO ()
withBH cb = bracket create cb delete
  where
    indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0)
    create = do
      bhEnv <- Q.mkEnv "http://localhost:9200"
      -- todo: generate random name
      let testIndex = BH.IndexName "test-index"
      BH.runBH bhEnv $ do
        _ <- BH.createIndex indexSettings testIndex
        _ <- BH.putMapping testIndex C.ChangesIndexMapping
        True <- BH.indexExists testIndex
        pure (bhEnv, testIndex)
    delete (bhEnv, testIndex) = do
      BH.runBH bhEnv $ do
        _ <- BH.deleteIndex testIndex
        False <- BH.indexExists testIndex
        pure ()

testIndexChange :: Assertion
testIndexChange = withBH doTest
  where
    checkIndex bhEnv testIndex = do
      let query = BH.MatchAllQuery Nothing
      let search = BH.mkSearch (Just query) Nothing
      parsed <- BH.runBH bhEnv $ do
        resp <- BH.searchByIndex testIndex search
        BH.parseEsResponse resp :: BH.BH IO (Either BH.EsError (BH.SearchResult Value))
      case parsed of
        Left _e -> error "Could not get changes back"
        Right sr -> assertEqual "changes" (BH.hitsTotal (BH.searchHits sr)) (BH.HitsTotal 1 BH.HTR_EQ)

    doTest :: (BH.BHEnv, BH.IndexName) -> IO ()
    doTest (bhEnv, testIndex) = do
      C.indexChanges bhEnv testIndex [fakeChange]
      checkIndex bhEnv testIndex
      C.indexChanges bhEnv testIndex [fakeChange]
      checkIndex bhEnv testIndex
