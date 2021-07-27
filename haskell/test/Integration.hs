module Main (main) where

import Monocle.Backend.Test
import Relude
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain (testGroup "Tests" [monocleIntegrationTests])

monocleIntegrationTests :: TestTree
monocleIntegrationTests =
  testGroup
    "Monocle.Backend.Changes"
    [ testCase
        "Index changes"
        testIndexChanges,
      testCase "Test achievement" testAchievements,
      testCase "Test reposSummary" testReposSummary,
      testCase "Test top authors" testTopAuthors,
      testCase "Test authors peers strength" testGetAuthorsPeersStrength,
      testCase "Test newContributors" testGetNewContributors,
      testCase
        "Index ProjectCrawlerMetadata"
        testProjectCrawlerMetadata,
      testCase
        "Index OrganizationCrawlerMetadata"
        testOrganizationCrawlerMetadata
    ]
