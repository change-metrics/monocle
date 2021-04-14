{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille
import Lentille.MonocleMock
import Relude
import qualified Streaming.Prelude as S
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [workerTests, monocleClientTests])

workerTests :: TestTree
workerTests =
  testGroup
    "Lentille.Worker"
    [testRun]

fakeTD :: TrackerData
fakeTD =
  TrackerData
    (IsoTime (fromMaybe (error "Oops") $ readMaybe "2021-04-01 00:00:00 UTC"))
    "changeUrl"
    "type"
    42
    "issueUrl"
    "issueTitle"
    "sev"
    "pri"

testRun :: TestTree
testRun = testCase "run" go
  where
    go = withMockClient $ \client -> do
      run
        client
        (ApiKey "fake")
        (IndexName "openstack")
        (CrawlerName "lentille")
        (TrackerDataFetcher (const $ S.each [fakeTD]))

monocleClientTests :: TestTree
monocleClientTests =
  testGroup
    "Lentille.Client"
    [ testGetIndices,
      testGetUpdatedSince,
      testPostData
    ]

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

testPostData :: TestTree
testPostData = testCase "postData" go
  where
    go = withMockClient $ \client -> do
      res <- postTrackerData client (IndexName "test") (CrawlerName "test") (ApiKey "failme") []
      assertEqual "Call failed" ["42"] res
