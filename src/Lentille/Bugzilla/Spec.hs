module Lentille.Bugzilla.Spec (bzClientTests) where

import Data.Vector qualified as V
import Lentille.Bugzilla
import Lentille.Bugzilla.Mock
import Monocle.Protob.Search (TaskData (..))
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import Web.RedHatBugzilla qualified as BZ

bzClientTests :: TestTree
bzClientTests =
  testGroup
    "Bugzilla.Mock"
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
      bz <- getBugWithScore bzSession 1791815
      case toTaskData bz of
        (td : _tds) ->
          sequence_
            [ taskDataTid td @=? "1791815",
              taskDataChangeUrl td @=? "https://review.opendev.org/764427",
              taskDataUrl td @=? "https://bugzilla.redhat.com/show_bug.cgi?id=1791815",
              taskDataTtype td @=? V.fromList ["FutureFeature"],
              taskDataScore td @=? 9001
            ]
        [] -> assertBool "No external bugs found" False

testSearchBugs :: TestTree
testSearchBugs = testCase "searchBugs" go
  where
    sinceTS = fromMaybe (error "Oops") $ readMaybe "2021-04-01 00:00:00 UTC"
    go = do
      bzSession <- bugzillaMockClient
      bugs <- BZ.searchBugsAll bzSession (searchExpr sinceTS "")
      -- print (length $ bugs)
      -- print (head <$> nonEmpty bugs)
      assertBool "Got bugs" (not . null $ bugs)
