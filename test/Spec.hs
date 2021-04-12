{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille
import Lentille.BugzillaMock
import Lentille.MonocleMock
import Network.HTTP.Mock (withMockedManager)
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.Bugzilla.RedHat as BZ

main :: IO ()
main = defaultMain (testGroup "Tests" [workerTests, bzClientTests, monocleClientTests])

workerTests :: TestTree
workerTests =
  testGroup
    "Lentille.Worker"
    [testRun]

testRun :: TestTree
testRun = testCase "run" go
  where
    go = withMockClient $ \client -> do
      bzSession <- bugzillaMockClient
      run bzSession client (ApiKey "fake") (IndexName "openstack") (CrawlerName "lentille")

bzClientTests :: TestTree
bzClientTests =
  testGroup
    "BugzillaMock"
    [testSearchBugs, testGetBug]

testGetBug :: TestTree
testGetBug = testCase "getBug" go
  where
    go = do
      bzSession <- bugzillaMockClient
      Just bug' <- BZ.getBug bzSession 1791815
      -- print bug'
      assertBool "Got bug ids" (isJust $ BZ.bugExternalBugs bug')

testSearchBugs :: TestTree
testSearchBugs = testCase "searchBugs" go
  where
    sinceTS = fromMaybe (error "Oops") $ readMaybe "2021-04-01 00:00:00 UTC"
    go = do
      bzSession <- bugzillaMockClient
      bugs <- BZ.searchBugsAll bzSession (searchExpr sinceTS)
      -- print (length $ bugs)
      -- print (head <$> nonEmpty bugs)
      assertBool "Got bugs" (not . null $ bugs)

monocleClientTests :: TestTree
monocleClientTests =
  testGroup
    "Lentille.Client"
    [ testGetIndices,
      testGetUpdatedSince
    ]

withMockClient :: (MonocleClient -> IO ()) -> IO ()
withMockClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

testGetUpdatedSince :: TestTree
testGetUpdatedSince = testCase "getUpdatedSince" go
  where
    go = withMockClient $ \client -> do
      lastUpdated <- getUpdatedSince client (IndexName "test") (CrawlerName "test")
      putText (show lastUpdated)
      assertBool "Got update" True

testGetIndices :: TestTree
testGetIndices = testCase "getIndices" go
  where
    go = withMockClient $ \client -> do
      indices <- getIndices client
      assertBool "Got indicies" (indices == ["indice1", "indice2"])
