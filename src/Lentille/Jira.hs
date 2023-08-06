-- Note [Record Dot Syntax]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- OverloadedRecordDot syntax enables accessing attribute that way: `obj.name`.
-- If the data type constructor is not in scope, GHC 9.2.4 produces this error:
--
--     • No instance for (GHC.Records.HasField "name" r Text)
--        arising from selecting the field ‘name’
--
-- This is because GHC relies on the HasField typeclass instance to get the attribute.
-- Thus, make sure that the data type definition is in scope when using the new syntax.
{-# LANGUAGE DeriveAnyClass #-}

module Lentille.Jira (
  JiraClient,
  newJiraClient,
  runJiraEffects,
  JQL (..),
  getIssueBodies,
  getIssues,
  getIssue,
  JiraIssue (..),
  JiraIssueBody (..),
) where

import Data.Aeson
import Data.Text qualified as T
import ListT qualified
import Network.HTTP.Client qualified as HTTP

import Control.Lens (toListOf, (^?))
import Data.Aeson.Lens (key, values, _String)

import Effectful.Prometheus qualified as E
import Effectful.Retry qualified as E
import Lentille
import Monocle.Effects (HttpEffect, httpRequest, httpRetry)
import Monocle.Effects qualified as E
import Monocle.Logging qualified as E
import Monocle.Prelude

data JiraClient = JiraClient
  { baseUrl :: Text
  , token :: Secret
  }

newJiraClient :: Text -> Secret -> JiraClient
newJiraClient url token = JiraClient {..}
 where
  baseUrl = T.dropWhileEnd (== '/') url <> "/rest/api/2/"

type JiraEffects es = (HttpEffect :> es, LoggerEffect :> es, PrometheusEffect :> es, Retry :> es)

runJiraEffects :: HTTP.Manager -> Eff [HttpEffect, LoggerEffect, PrometheusEffect, Retry, IOE] a -> IO a
runJiraEffects manager = runEff . E.runRetry . E.runPrometheus . E.runLoggerEffect . E.runHttpEffectWithManager manager

httpJSONRequest :: JiraEffects es => HTTP.Request -> Eff es (Either Text Value)
httpJSONRequest request =
  either (Left . from) Right
    . eitherDecode
    . HTTP.responseBody
    <$> httpRetry (decodeUtf8 request.path) (httpRequest request)

jiraRequest :: JiraEffects es => JiraClient -> Text -> ByteString -> HTTP.RequestBody -> Eff es (Either Text Value)
jiraRequest client path verb body = do
  initRequest <- HTTP.parseUrlThrow (from $ client.baseUrl <> path)
  let request =
        initRequest
          { HTTP.requestHeaders =
              [ ("Content-Type", "application/json")
              , bearerTokenHeader client.token
              ]
          , HTTP.method = verb
          , HTTP.requestBody = body
          }
  httpJSONRequest request

newtype JiraID = JiraID Text deriving newtype (ToJSON)

data JiraIssue = JiraIssue
  { jid :: JiraID
  , name :: Text
  , updated :: UTCTime
  }
  deriving (ToJSON, Generic)

-- | Drop the extra milli second and timezone
--
-- >>> parseJiraTime "2022-09-13T14:37:36.000+0000"
-- Just 2022-09-13 14:37:36 UTC
parseJiraTime :: Text -> Maybe UTCTime
parseJiraTime t = parseDateValue (from $ T.takeWhile (/= '.') t <> "Z")

decodeIssue :: Value -> Either Text JiraIssue
decodeIssue v = do
  jid <- (JiraID <$> v ^? key "id" . _String) `pDie` "Can't find id"
  name <- (v ^? key "key" . _String) `pDie` "Can't find key"
  updatedString <- (v ^? key "fields" . key "updated" . _String) `pDie` "Can't find fields.updated"
  updated <- parseJiraTime updatedString `pDie` ("Can't parse date: " <> updatedString)
  pure (JiraIssue {..})
 where
  pDie :: Maybe a -> Text -> Either Text a
  pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (encode v))

data JiraIssueBody = JiraIssueBody
  { project :: Text
  , name :: Text
  , issueType :: Text
  , updated :: UTCTime
  , description :: Maybe Text
  , summary :: Text
  }
  deriving (ToJSON, Generic)

decodeIssueBody :: Value -> Either Text JiraIssueBody
decodeIssueBody v = do
  name <- (v ^? key "key" . _String) `pDie` "Can't find kid"
  fields <- (v ^? key "fields") `pDie` "Can't find fields"
  project <- (fields ^? key "project" . key "key" . _String) `pDie` "Can't find project.key"
  issueType <- (fields ^? key "issuetype" . key "name" . _String) `pDie` "Can't find issuetype.name"
  updated <- (parseJiraTime =<< fields ^? key "updated" . _String) `pDie` "Can't find updated"
  let description = fields ^? key "description" . _String
  summary <- (fields ^? key "summary" . _String) `pDie` "Can't find summary"
  pure JiraIssueBody {..}
 where
  pDie :: Maybe a -> Text -> Either Text a
  pDie a n = a `orDie` (n <> ": " <> decodeUtf8 (encode v))

getIssue :: JiraEffects es => JiraClient -> JiraID -> Eff es (Either Text JiraIssueBody)
getIssue client (JiraID jid) = do
  res <- jiraRequest client ("issue/" <> jid) "GET" mempty
  pure $ case res of
    Left e -> Left (jid <> ": " <> from e)
    Right x -> decodeIssueBody x

newtype JQL = JQL Text

getIssues :: forall es. JiraEffects es => JiraClient -> UTCTime -> JQL -> ListT (Eff es) (Either Text JiraIssue)
getIssues client sinceTS (JQL query) = do
  let body start =
        object
          [ ("maxResults", Number 100)
          , ("startAt", Number (fromInteger start))
          , ("fields", Array (fromList [String "updated"]))
          , ("jql", String $ query <> " order by updated")
          ]

  let abortList :: Text -> ListT (Eff es) (Either Text JiraIssue)
      abortList t = ListT.cons (Left t) mempty

  let getUntilTS :: Integer -> ListT (Eff es) (Either Text JiraIssue)
      getUntilTS start = do
        -- Get the results starting from start
        searchResult <- lift $ jiraRequest client "search" "POST" (HTTP.RequestBodyLBS (encode $ body start))

        let yieldIssues :: Integer -> Integer -> [Value] -> ListT (Eff es) (Either Text JiraIssue)
            yieldIssues total count = \case
              []
                | -- We reach the end of the results -> we stop
                  count >= total ->
                    mempty
                | -- Otherwise we continue from the current count
                  otherwise ->
                    getUntilTS count
              (x : xs) -> case decodeIssue x of
                Left e -> abortList $ "Invalid issue: " <> e
                Right issue
                  | -- We reach the last updated since limit -> we stop
                    issue.updated < sinceTS ->
                      mempty
                  | -- Otherwise we add the element to the list and we continue
                    otherwise ->
                      ListT.cons (Right issue) (yieldIssues total (count + 1) xs)

        -- Decode the issues list and the total available results
        case searchResult of
          Left e -> abortList $ "Invalid response: " <> e
          Right x -> case (x ^? key "total" . _Integer, toListOf (key "issues" . values) x) of
            (_, []) -> abortList $ "Couldn't find issues: " <> show x
            (Nothing, _) -> abortList $ "Couldn't find total: " <> show x
            (Just total, issues) -> yieldIssues total start issues

  getUntilTS 0

getIssueBodies :: forall es. JiraEffects es => JiraClient -> UTCTime -> JQL -> ListT (Eff es) (Either Text JiraIssueBody)
getIssueBodies client sinceTS jql = ListT.traverse getIssueBody (getIssues client sinceTS jql)
 where
  getIssueBody :: Either Text JiraIssue -> Eff es (Either Text JiraIssueBody)
  getIssueBody jiraIssue = case jiraIssue of
    Left e -> pure $ Left e
    Right x -> getIssue client x.jid
