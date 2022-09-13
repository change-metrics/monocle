module Lentille.JiraSpec (jiraClientTests, main) where

import Data.ByteString qualified as BS
import ListT qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Mock (withMockedManager)
import Network.HTTP.Types.Status (status200)
import Network.Wai qualified as Wai
import Test.Tasty
import Test.Tasty.HUnit

import Effectful.Fail qualified as E
import Monocle.Prelude

import Lentille.Jira qualified as Jira

jiraClientTests :: TestTree
jiraClientTests = testGroup "Lentille.Jira" [testGetIssues, testGetIssue]

withMockClient :: (HTTP.Manager -> IO ()) -> IO ()
withMockClient cb = do
  searchResp <- BS.readFile "test/data/jirasearch.json"
  issueResp <- BS.readFile "test/data/jiraissue.json"
  let app req respond = respond . Wai.responseLBS status200 mempty . from $ case Wai.rawPathInfo req of
        "/rest/api/2/search" -> searchResp
        "/rest/api/2/issue/14825490" -> issueResp
        other -> error $ "Invalid path: " <> show other
  withMockedManager app cb

testGetIssues :: TestTree
testGetIssues = testCase "getIssues" go
 where
  go = withMockClient $ \manager -> do
    Jira.runJiraEffects manager do
      let client = Jira.newJiraClient "http://localhost" (from @Text "test-token")
      let issues = Jira.getIssues client [utctime|2022-01-01 00:00:00|] (Jira.JQL "")
      xs <- ListT.toList issues
      unsafeEff_ $ length xs @?= 2

      let issues' = Jira.getIssues client [utctime|2022-09-01 00:00:00|] (Jira.JQL "")
      ys <- ListT.toList issues'
      unsafeEff_ $ length ys @?= 1

testGetIssue :: TestTree
testGetIssue = testCase "getIssue" go
 where
  go = withMockClient $ \manager -> do
    Jira.runJiraEffects manager $ E.runFailIO do
      let client = Jira.newJiraClient "http://localhost" (from @Text "test-token")
      Just (Right issue) <- ListT.head $ Jira.getIssues client [utctime|2022-01-01 00:00:00|] (Jira.JQL "")
      issueBody <- Jira.getIssue client issue.jid
      unsafeEff_ $ case issueBody of
        Left x -> error ("Decode failed: " <> x)
        Right x -> do
          x.issueType @?= "Story"
          x.project @?= "PROJECT"
          x.name @?= issue.name

main :: IO ()
main = defaultMain jiraClientTests
