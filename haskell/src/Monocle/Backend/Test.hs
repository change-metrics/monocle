{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Test where

import Control.Exception (bracket)
import Data.Aeson (Value)
import qualified Database.Bloodhound as BH
import qualified Monocle.Backend.Index as C
import qualified Monocle.Search.Queries as Q
import Relude
import Test.Tasty.HUnit

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
      C.indexChanges bhEnv testIndex [C.Change "test change" "test-author" "001"]
      checkIndex bhEnv testIndex
      C.indexChanges bhEnv testIndex [C.Change "test change" "test-author" "001"]
      checkIndex bhEnv testIndex
