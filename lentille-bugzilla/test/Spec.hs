{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille
import Lentille.Bugzilla
import Lentille.BugzillaMock
import Lentille.MonocleMock
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.Bugzilla.RedHat as BZ

main :: IO ()
main = defaultMain (testGroup "Tests" [workerTests, bzClientTests])

workerTests :: TestTree
workerTests =
  testGroup
    "Lentille.Worker"
    [testRun]

testRun :: TestTree
testRun = testCase "run" go
  where
    go = withMockClient withClient $ \client -> do
      bzSession <- bugzillaMockClient
      run
        client
        Nothing
        (ApiKey "fake")
        (IndexName "openstack")
        (CrawlerName "lentille")
        (TaskDataFetcher (getBZData bzSession))

bzClientTests :: TestTree
bzClientTests =
  testGroup
    "BugzillaMock"
    [testSearchBugs, testGetBug, testBugToTaskData]

testGetBug :: TestTree
testGetBug = testCase "getBug" go
  where
    go = do
      bzSession <- bugzillaMockClient
      Just bug' <- BZ.getBug bzSession 1791815
      -- print bug'
      assertBool "Got bug ids" (isJust $ BZ.bugExternalBugs bug')

testBugToTaskData :: TestTree
testBugToTaskData = testCase "bugToTaskData" go
  where
    go = do
      bzSession <- bugzillaMockClient
      Just bz <- BZ.getBug bzSession 1791815
      case toTaskData bz of
        (td : _tds) ->
          sequence_
            [ tdTid td @=? "1791815",
              tdChangeUrl td @=? "https://review.opendev.org/764427",
              tdUrl td @=? "https://bugzilla.redhat.com/show_bug.cgi?id=1791815",
              tdTtype td @=? ["FutureFeature"]
            ]
        [] -> assertBool "No external bugs found" False

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
