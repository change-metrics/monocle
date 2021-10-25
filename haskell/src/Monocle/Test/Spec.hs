module Monocle.Test.Spec (main) where

import qualified Data.Aeson.Encode.Pretty as Aeson
import Lentille.Bugzilla.Spec
import Macroscope.Test (monocleMacroscopeTests)
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Provisioner (runProvisioner)
import Monocle.Backend.Test
import Monocle.Env
import Monocle.Prelude
import qualified Monocle.Search.Lexer as L
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import qualified Monocle.Search.Syntax as S
import System.Environment (setEnv)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  setEnv "API_KEY" "secret"
  setEnv "CRAWLERS_API_KEY" "secret"
  integrationTests <- do
    elasticUrl <- lookupEnv "ELASTIC_URL"
    case elasticUrl of
      Nothing -> do
        putTextLn $ "ELASTIC_URL is missing, we skip integration test"
        pure []
      Just _ -> do
        setEnv "TASTY_NUM_THREADS" "1"
        pure [monocleIntegrationTests, monocleMacroscopeTests]

  provisionerM <- lookupEnv "PROVISIONER"
  case provisionerM of
    Just provisioner -> runProvisioner (toText provisioner) >> exitSuccess
    Nothing -> pure ()

  defaultMain
    ( testGroup "Tests" $
        [ monocleSearchLanguage,
          monocleConfig,
          bzClientTests
        ]
          <> integrationTests
    )

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
      testCase "Test changes top" testGetChangesTops,
      testCase "Test authors peers strength" testGetAuthorsPeersStrength,
      testCase "Test newContributors" testGetNewContributors,
      testCase "Test getActivityStats" testGetActivityStats,
      testCase
        "Index ProjectCrawlerMetadata"
        testProjectCrawlerMetadata,
      testCase
        "Index OrganizationCrawlerMetadata"
        testOrganizationCrawlerMetadata,
      testCase
        "Index TaskDataCrawlerMetadata"
        testTaskDataCrawlerMetadata,
      testCase "Test suggestions" testGetSuggestions,
      testCase "Test taskData add" testTaskDataAdd,
      testCase "Test taskData adoption" testTaskDataAdoption,
      testCase "Test Janitor wipe crawler" testJanitorWipeCrawler
    ]

monocleSearchLanguage :: TestTree
monocleSearchLanguage =
  testGroup
    "Monocle.Search"
    [ testCase
        "Lexer basic"
        ( lexMatch "state:open" [L.Literal "state", L.Equal, L.Literal "open"]
        ),
      testCase
        "Parser basic"
        (parseMatch "state:open" (S.EqExpr "state" "open")),
      testCase
        "Lexer paren"
        ( lexMatch
            "(a>42 or a:0 ) and b:d"
            [ L.OpenParenthesis,
              L.Literal "a",
              L.Greater,
              L.Literal "42",
              L.Or,
              L.Literal "a",
              L.Equal,
              L.Literal "0",
              L.CloseParenthesis,
              L.And,
              L.Literal "b",
              L.Equal,
              L.Literal "d"
            ]
        ),
      testCase
        "Lexer quoted"
        (lexMatch "field:\"A value\"" [L.Literal "field", L.Equal, L.Literal "A value"]),
      testCase
        "Lexer unicode"
        (lexMatch "not field:λr🌈bow" [L.Not, L.Literal "field", L.Equal, L.Literal "λr🌈bow"]),
      testCase
        "Lexer quoted unicode"
        (lexMatch "\"Zuul ▲ user\"" [L.Literal "Zuul ▲ user"]),
      testCase
        "Lexer does not escape"
        (lexMatch "\"test\\\"" [L.Literal "test\\"]),
      testCase
        "Error basic"
        (errMatch "a" 1 "Expected operator (`:`, `>`, ...)"),
      testCase
        "Error simple"
        (errMatch "a:" 2 "Expected value"),
      testCase
        "Error typo"
        (errMatch "a:42 nor b:23" 5 "Unexpected token"),
      testCase
        "Error missing right expr"
        (errMatch "a:41 or" 7 "Expected expression"),
      testCase
        "Error not expr"
        (errMatch "a:41 not" 8 "Expected expression"),
      testCase
        "Error space"
        (errMatch "a: b" 2 "Expected field value (spaces are not allowed after operator)"),
      testCase
        "Error ("
        (errMatch "(a:42" 5 "Expected closing paren `)`"),
      testCase
        "Error ("
        (errMatch "a:42)" 5 "Missing opening paren `(`"),
      testCase
        "Error field"
        (errMatch "bad:cofee" 0 "Unknown field: bad"),
      testCase
        "Error value"
        (errMatch "from:hier" 0 "Invalid date: hier"),
      testCase
        "Parser paren"
        ( parseMatch
            "(a>42 or a:0) and b:d"
            ( (S.AndExpr (S.OrExpr (S.GtExpr "a" "42") (S.EqExpr "a" "0")) (S.EqExpr "b" "d"))
            )
        ),
      testCase
        "Parser aliases"
        ( parseMatch'
            [("sprint42", S.GtExpr "date" "2021-07-01")]
            "status:open sprint42"
            ( (S.AndExpr (S.EqExpr "status" "open")) (S.GtExpr "date" "2021-07-01")
            )
        ),
      testCase
        "Parser implicit and"
        ( parseMatch
            "state:open author:foo"
            ( (S.AndExpr (S.EqExpr "state" "open") (S.EqExpr "author" "foo"))
            )
        ),
      testCase
        "Query date"
        ( queryMatch
            "updated_at>2021-05-27"
            "{\"range\":{\"updated_at\":{\"boost\":1,\"gt\":\"2021-05-27T00:00:00Z\"}}}"
        ),
      testCase
        "Query number"
        ( queryMatch
            "score>200"
            "{\"range\":{\"tasks_data.score\":{\"boost\":1,\"gt\":200}}}"
        ),
      testCase
        "Query boolean"
        ( queryMatch
            "not state:self_merged"
            "{\"bool\":{\"must_not\":[{\"term\":{\"self_merged\":{\"value\":\"true\"}}}]}}"
        ),
      testCase
        "Query regex"
        ( queryMatch
            "repo:openstack/.*nova.*"
            "{\"regexp\":{\"repository_fullname\":{\"flags\":\"ALL\",\"value\":\"openstack/.*nova.*\"}}}"
        ),
      testCase
        "Query state"
        ( queryMatch
            "state:abandoned"
            "{\"term\":{\"state\":{\"value\":\"CLOSED\"}}}"
        ),
      testCase
        "Query date"
        ( queryMatch
            "updated_at>2021 and updated_at<2021-05"
            "{\"bool\":{\"must\":[{\"range\":{\"updated_at\":{\"boost\":1,\"gt\":\"2021-01-01T00:00:00Z\"}}},{\"range\":{\"updated_at\":{\"boost\":1,\"lt\":\"2021-05-01T00:00:00Z\"}}}]}}"
        ),
      testCase
        "Query relative date"
        ( queryMatch
            "updated_at>now-3weeks"
            "{\"range\":{\"updated_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}}"
        ),
      testCase
        "Query from field"
        ( queryMatch
            "from:now-3weeks"
            "{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}}"
        ),
      testCase
        "Query from flavor"
        ( queryMatchFlavor
            (Q.QueryFlavor Q.Author Q.UpdatedAt)
            "from:now-3weeks"
            "{\"range\":{\"updated_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}}"
        ),
      testCase
        "Query from to field"
        ( queryMatch
            "from:now-3weeks to:now"
            "{\"bool\":{\"must\":[{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}},{\"range\":{\"created_at\":{\"boost\":1,\"lt\":\"2021-05-31T00:00:00Z\"}}}]}}"
        ),
      testCase
        "Query label field"
        ( queryMatch
            "label:bug"
            "{\"term\":{\"labels\":{\"value\":\"bug\"}}}"
        ),
      testCase
        "Query project"
        ( queryMatch
            "project:zuul"
            "{\"bool\":{\"must\":[{\"regexp\":{\"repository_fullname\":{\"flags\":\"ALL\",\"value\":\"zuul/.*\"}}},{\"regexp\":{\"target_branch\":{\"flags\":\"ALL\",\"value\":\"master\"}}},{\"regexp\":{\"changed_files.path\":{\"flags\":\"ALL\",\"value\":\"tests/.*\"}}}]}}"
        ),
      testCase
        "Query author"
        ( queryMatchFlavor
            (Q.QueryFlavor Q.Author Q.UpdatedAt)
            "author:alice"
            "{\"regexp\":{\"author.muid\":{\"flags\":\"ALL\",\"value\":\"alice\"}}}"
        ),
      testCase
        "Query on author"
        ( queryMatchFlavor
            (Q.QueryFlavor Q.OnAuthor Q.UpdatedAt)
            "author:alice"
            "{\"regexp\":{\"on_author.muid\":{\"flags\":\"ALL\",\"value\":\"alice\"}}}"
        ),
      testCase
        "Query multi-or"
        ( queryMatch
            "score:1 (score:2 or score:3 or score:4) score:5 score:6"
            "{\"bool\":{\"must\":[{\"term\":{\"tasks_data.score\":{\"value\":\"1\"}}},{\"bool\":{\"should\":[{\"term\":{\"tasks_data.score\":{\"value\":\"2\"}}},{\"term\":{\"tasks_data.score\":{\"value\":\"3\"}}},{\"term\":{\"tasks_data.score\":{\"value\":\"4\"}}}]}},{\"term\":{\"tasks_data.score\":{\"value\":\"5\"}}},{\"term\":{\"tasks_data.score\":{\"value\":\"6\"}}}]}}"
        ),
      testCase
        "Query default bound"
        (queryMatchBound "state:open" (threeWeek, now)),
      testCase
        "Query lower bound"
        (queryMatchBound "updated_at>2021-01-01" (d "2021-01-01", now)),
      testCase
        "Query max bound"
        (queryMatchBound "updated_at<2021-01-01" (d "2020-12-11", d "2021-01-01")),
      testCase
        "Query both bounds"
        (queryMatchBound "created_at<2020-08-01 and updated_at>2020-06-01" (d "2020-06-01", d "2020-08-01")),
      testCase
        "Query silly bounds"
        (queryMatchBound "created_at<2020-01-01 and updated_at>2021-01-01" (d "2021-01-01", d "2020-01-01")),
      testCase
        "Query multi bounds"
        ( queryMatchBound
            "created_at>2000 and created_at>2001 and created_at<2010 and created_at<2011"
            (d "2001-01-01", d "2010-01-01")
        ),
      testCase "QueryM withFlavor" testWithFlavor,
      testCase "QueryM combinator" testSimpleQueryM,
      testCase "QueryM ensureMinBound" testEnsureMinBound,
      testCase "QueryM dropDate" testDropDate
    ]
  where
    mkQueryM code = testQueryM testTenant . withQuery (mkCodeQuery code)

    testWithFlavor :: Assertion
    testWithFlavor = mkQueryM "from:2021" $ do
      withFlavor (Q.QueryFlavor Q.Author Q.OnCreatedAndCreated) $ do
        q <- prettyQuery
        let expected = "{\"bool\":{\"filter\":[{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2021-01-01T00:00:00Z\"}}},{\"range\":{\"on_created_at\":{\"boost\":1,\"gt\":\"2021-01-01T00:00:00Z\"}}}]}}"
        liftIO $ assertEqual "OnCreatedAndCreated range flavor" (Just expected) q
        withFlavor (Q.QueryFlavor Q.Author Q.UpdatedAt) $ do
          q' <- prettyQuery
          let expected' = "{\"range\":{\"updated_at\":{\"boost\":1,\"gt\":\"2021-01-01T00:00:00Z\"}}}"
          liftIO $ assertEqual "range flavor reset" (Just expected') q'

    testSimpleQueryM :: Assertion
    testSimpleQueryM = mkQueryM "author:alice" $ do
      q <- prettyQuery
      liftIO $ assertEqual "simple queryM work" (Just "{\"regexp\":{\"author.muid\":{\"flags\":\"ALL\",\"value\":\"alice\"}}}") q
      dropQuery $ do
        emptyQ <- prettyQuery
        liftIO $ assertEqual "dropQuery work" Nothing emptyQ
        withModified (const $ Just (S.EqExpr "author" "bob")) $ do
          newQ <- prettyQuery
          liftIO $ assertEqual "withModified work" (Just "{\"regexp\":{\"author.muid\":{\"flags\":\"ALL\",\"value\":\"bob\"}}}") newQ

    testEnsureMinBound :: Assertion
    testEnsureMinBound = do
      testQueryM testTenant $ do
        withQuery (Q.ensureMinBound $ mkCodeQuery "author:alice") $ do
          got <- prettyQuery
          let expected = "{\"bool\":{\"must\":[{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}},{\"regexp\":{\"author.muid\":{\"flags\":\"ALL\",\"value\":\"alice\"}}}]}}"
          liftIO $ assertEqual "bound ensured with query" (Just expected) got
        withQuery (Q.ensureMinBound $ mkCodeQuery "") $ do
          got <- prettyQuery
          let expected = "{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2021-05-10T00:00:00Z\"}}}"
          liftIO $ assertEqual "match ensured without query" (Just expected) got

    testDropDate :: Assertion
    testDropDate = mkQueryM "from:2020 repo:zuul" $ do
      got <- prettyQuery
      let expected = "{\"bool\":{\"must\":[{\"range\":{\"created_at\":{\"boost\":1,\"gt\":\"2020-01-01T00:00:00Z\"}}},{\"regexp\":{\"repository_fullname\":{\"flags\":\"ALL\",\"value\":\"zuul\"}}}]}}"
      liftIO $ assertEqual "match" (Just expected) got
      withModified Q.dropDate $ do
        newQ <- prettyQuery
        liftIO $ assertEqual "drop date worked" (Just "{\"regexp\":{\"repository_fullname\":{\"flags\":\"ALL\",\"value\":\"zuul\"}}}") newQ

    -- Get pretty query
    prettyQuery :: QueryM (Maybe LByteString)
    prettyQuery = fmap encodePretty <$> getQueryBH

    -- Create a Query object
    mkQuery' code = P.parse [] code >>= Q.queryWithMods now mempty testTenant
    mkCodeQuery code = case mkQuery' code of
      Left e -> error $ show e
      Right q -> q

    errMatch :: Text -> Int -> Text -> Assertion
    errMatch code pos msg = case mkQuery' code of
      Left e -> assertEqual "error matched" (S.ParseError msg pos) e
      Right _ -> error $ "Query didn't failed: " <> show code

    d :: String -> UTCTime
    d date = fromMaybe (error "nop") (readMaybe $ date <> " 00:00:00 Z")
    threeWeek = [utctime|2021-05-10 00:00:00|]
    now = [utctime|2021-05-31 00:00:00|]
    lexMatch code tokens = assertEqual "match" (Right tokens) (fmap L.token <$> L.lex code)
    parseMatch = parseMatch' []
    parseMatch' aliases code expr =
      assertEqual "match" (Right (Just expr)) (P.parse aliases code)
    queryDoMatch = queryDoMatch' []
    queryDoMatch' aliases field code query =
      assertEqual
        "match"
        (Right query)
        (P.parse aliases code >>= Q.queryWithMods now mempty testTenant >>= pure . field)
    encodePretty =
      Aeson.encodePretty'
        ( Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 0, Aeson.confCompare = compare @Text}
        )
    headS = \case
      [x] -> x
      _ -> error "Not a list"
    queryGet flavor query = Q.queryGet query id flavor
    queryMatch = queryDoMatch' [] (encodePretty . headS . queryGet Nothing)
    queryMatchFlavor flavor = queryDoMatch' [] (encodePretty . headS . queryGet (Just flavor))
    queryMatchBound = queryDoMatch Q.queryBounds
    testTenant =
      Config.Index
        { Config.name = "test",
          Config.projects = Just [testProjects],
          Config.search_aliases =
            ( Just
                [ let name = "sprint42"
                      alias = "from:2021-01-01 to:2021-01-21"
                   in Config.SearchAlias {..}
                ]
            ),
          Config.crawlers = [],
          Config.crawlers_api_key = Nothing,
          Config.idents = Nothing
        }
    testProjects =
      let br = Just "master"
          fr = Just "tests/.*"
          rr = Just "zuul/.*"
       in Config.Project br fr "zuul" rr

monocleConfig :: TestTree
monocleConfig =
  testGroup
    "Monocle.Api.Config"
    [testConfigLoad]
  where
    testConfigLoad = testCase "Decode config" $ do
      conf <- Config.loadConfig "./test/data/config.yaml"
      assertEqual "config is loaded" 1 (length conf)
