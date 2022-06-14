-- | The integrations tests entry point.
module Tests (main) where

import Data.Vector qualified as V
import Lentille.Bugzilla.Spec
import Macroscope.Test (monocleMacroscopeTests)
import Monocle.Api.Test (mkAppEnv, withTestApi)
import Monocle.Backend.Provisioner (runProvisioner)
import Monocle.Backend.Test
import Monocle.Client (MonocleClient (tokenM))
import Monocle.Client.Api (authGetMagicJwt, authWhoAmi, configGetGroupMembers, configGetGroups, crawlerCommitInfo)
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Prelude
import Monocle.Protob.Auth
import Monocle.Protob.Config
  ( GetGroupMembersRequest (GetGroupMembersRequest),
    GetGroupMembersResponse (GetGroupMembersResponse, getGroupMembersResponseMembers),
    GetGroupsRequest (GetGroupsRequest),
    GetGroupsResponse (GetGroupsResponse, getGroupsResponseItems),
    GroupDefinition (GroupDefinition),
  )
import Monocle.Protob.Crawler
import Monocle.Search.Lexer qualified as L
import Monocle.Search.Parser qualified as P
import Monocle.Search.Query qualified as Q
import Monocle.Search.Syntax qualified as S
import Proto3.Suite (Enumerated (Enumerated, enumerated))
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = withOpenSSL $ do
  setEnv "API_KEY" "secret"
  setEnv "CRAWLERS_API_KEY" "secret"
  integrationTests <- do
    elasticUrl <- lookupEnv "ELASTIC_URL"
    case elasticUrl of
      Nothing -> do
        putTextLn "ELASTIC_URL is missing, we skip integration test"
        pure []
      Just _ -> do
        setEnv "TASTY_NUM_THREADS" "1"
        pure
          [ monocleBackendQueriesTests,
            monocleMacroscopeTests,
            monocleApiTests
          ]

  -- TODO: move provisioner to the CLI module
  provisionerM <- lookupEnv "PROVISIONER"
  case provisionerM of
    Just provisioner -> runProvisioner (from provisioner) >> exitSuccess
    Nothing -> pure ()

  defaultMain
    ( testGroup "Tests" $
        [ monocleSearchLanguage,
          monocleConfig,
          bzClientTests
        ]
          <> integrationTests
    )

-- Create an AppEnv where the supply of a new config can be controled
mkAppEnvWithSideEffect :: Config.Config -> Config.Config -> TVar Bool -> IO AppEnv
mkAppEnvWithSideEffect config' newConfig reloadedRef = do
  bhEnv <- mkEnv'
  ws <- newMVar $ Config.mkWorkspaceStatus config'
  newWs <- newMVar $ Config.mkWorkspaceStatus newConfig
  jwk <- generateKey
  Config.setWorkspaceStatus Config.Ready ws
  let glLogger _ = pure ()
      config = configSE (config', ws) (newConfig, newWs)
      aOIDC = OIDC Nothing (defaultJWTSettings jwk)
      aEnv = Env {..}
  pure $ AppEnv {..}
  where
    configSE conf confNew = do
      reloaded <- readTVarIO reloadedRef
      let (config, wsRef) = if reloaded then confNew else conf
      pure $ Config.ConfigStatus reloaded config wsRef

pattern UnknownIndexResp :: CommitInfoResponse
pattern UnknownIndexResp <-
  CommitInfoResponse
    { commitInfoResponseResult =
        Just
          ( CommitInfoResponseResultError
              Enumerated {enumerated = Right CommitInfoErrorCommitGetUnknownIndex}
            )
    }

pattern NamedEntity :: LText -> CommitInfoResponse
pattern NamedEntity name <-
  CommitInfoResponse
    { commitInfoResponseResult =
        Just
          ( CommitInfoResponseResultEntity
              CommitInfoResponse_OldestEntity
                { commitInfoResponse_OldestEntityEntity =
                    Just
                      Entity {entityEntity = Just (EntityEntityProjectName name)},
                  commitInfoResponse_OldestEntityLastCommitAt = Just _
                }
            )
    }

pattern NoEntity :: CommitInfoResponse
pattern NoEntity <-
  CommitInfoResponse
    { commitInfoResponseResult =
        Just
          ( CommitInfoResponseResultError
              Enumerated {enumerated = Right CommitInfoErrorCommitGetNoEntity}
            )
    }

monocleApiTests :: TestTree
monocleApiTests =
  testGroup
    "Monocle.Api.Server"
    [ testCase "Test getGroups and getGroupMembers" testGetGroups,
      testCase "Test crawler MDs refreshed after config reload" testReloadedConfig,
      testCase "Test get metrics" testGetMetrics,
      testCase "Test Auth Magic Token endpoint" testAuthMagicTokenEndpoint
    ]
  where
    testAuthMagicTokenEndpoint :: Assertion
    testAuthMagicTokenEndpoint = do
      let appEnv = mkAppEnv $ Config.mkTenant "ws"
      let adminToken = "test"
      setEnv "ADMIN_TOKEN" adminToken
      withTestApi appEnv $ \_logger client -> do
        resp <- authGetMagicJwt client $ GetMagicJwtRequest $ from adminToken
        case resp of
          GetMagicJwtResponse (Just (GetMagicJwtResponseResultJwt jwt)) -> do
            let authClient = client {tokenM = Just $ from jwt}
            resp' <- authWhoAmi authClient $ WhoAmiRequest ""
            case resp' of
              WhoAmiResponse (Just (WhoAmiResponseResultUid au)) ->
                assertEqual
                  "Assert expected WhoAmI response"
                  "AUser {aMuidMap = fromList [], aDefaultMuid = \"bot\"}"
                  au
              _ -> error "Unexpected WhoAmI Response"
          _ -> error "Unexpected GetMagicJWT reponse"

    testGetGroups :: Assertion
    testGetGroups = do
      let appEnv =
            mkAppEnv $
              (Config.mkTenant "ws")
                { Config.idents =
                    Just
                      [ Config.Ident [] (Just ["grp1", "grp2"]) "John",
                        Config.Ident [] (Just ["grp2", "grp3"]) "Jane"
                      ]
                }
      withTestApi appEnv $ \_logger client ->
        do
          GetGroupsResponse {..} <- configGetGroups client $ GetGroupsRequest "ws"
          assertEqual
            "Validate getGroups"
            ( V.fromList
                [ GroupDefinition "grp1" 1,
                  GroupDefinition "grp2" 2,
                  GroupDefinition "grp3" 1
                ]
            )
            getGroupsResponseItems
          GetGroupMembersResponse {..} <- configGetGroupMembers client $ GetGroupMembersRequest "ws" "grp2"
          assertEqual "Validate getGroupMembers" (V.fromList ["Jane", "John"]) getGroupMembersResponseMembers
      assertEqual "Validate getGroups and getGroupMembers" True True

    testReloadedConfig :: Assertion
    testReloadedConfig = do
      reloadedRef <- newTVarIO False
      let appEnv =
            mkAppEnvWithSideEffect
              (Config.Config Nothing Nothing [makeFakeWS wsName1 ["opendev/neutron"]])
              ( Config.Config
                  Nothing
                  Nothing
                  [ makeFakeWS wsName1 ["opendev/neutron", "opendev/nova"],
                    makeFakeWS wsName2 ["opendev/swift"]
                  ]
              )
              reloadedRef
      withTestApi appEnv $ \_logger client ->
        do
          -- Run a commitInfo and expect an Entity
          resp1 <- crawlerCommitInfo client $ mkReq wsName1 0
          if not $ isEntityNeutron resp1
            then error "Expected entity name 'openstack/neutron'"
            else pure ()
          -- Run a commitInfo (with an offset) on first workspace and expect noEntity
          resp2 <- crawlerCommitInfo client $ mkReq wsName1 1
          if not $ isNoEntity resp2 then error "Expected noEntity response" else pure ()
          -- Also perform the Req on an unknown workspace (and expect failure)
          resp3 <- crawlerCommitInfo client $ mkReq wsName2 0
          if not $ isUnknownIndex resp3 then error "Expected UnknownIndex response" else pure ()

          -- Now set: config has been reloaded and serve the alternate config
          atomically $ writeTVar reloadedRef True
          -- Run two commitInfo requests (and expect different resp
          -- as the new config as been handled and crawler MD has been refreshed)
          resp1' <- crawlerCommitInfo client $ mkReq wsName1 0
          resp2' <- crawlerCommitInfo client $ mkReq wsName1 1
          if resp1' == resp2' then error "Expected different response" else pure ()
          if isNoEntity resp2' then error "Expected an Entity" else pure ()
          -- Also verify that the new workspace was initilized
          resp3' <- crawlerCommitInfo client $ mkReq wsName2 0
          if not $ isEntitySwift resp3'
            then error "Expected entity named 'openstack/swift'"
            else pure ()
      assertEqual "Crawler MD reload when config change" True True
      where
        isUnknownIndex UnknownIndexResp = True
        isUnknownIndex _ = False
        isNoEntity NoEntity = True
        isNoEntity _ = False
        isEntitySwift (NamedEntity "opendev/swift") = True
        isEntitySwift _ = False
        isEntityNeutron (NamedEntity "opendev/neutron") = True
        isEntityNeutron _ = False
        mkReq wsName offset =
          let commitInfoRequestIndex = from wsName
              commitInfoRequestCrawler = from crawlerName
              commitInfoRequestEntity = Just . Entity . Just $ EntityEntityProjectName ""
              commitInfoRequestOffset = offset
           in CommitInfoRequest {..}
        makeFakeWS wsName repositories =
          (mkConfig wsName)
            { Config.crawlers_api_key = Just "secret",
              Config.crawlers =
                [ let name = crawlerName
                      update_since = "2000-01-01"
                      provider =
                        Config.GerritProvider
                          ( Config.Gerrit
                              { gerrit_login = Nothing,
                                gerrit_password = Nothing,
                                gerrit_prefix = Nothing,
                                gerrit_repositories = Just repositories,
                                gerrit_url = "https://fake.url"
                              }
                          )
                   in Config.Crawler {..}
                ]
            }
        wsName1 = "ws1"
        wsName2 = "ws2"
        crawlerName = "testy"

monocleBackendQueriesTests :: TestTree
monocleBackendQueriesTests =
  testGroup
    "Monocle.Backend.Queries"
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
      testCase "Test getAllAuthorsMuid" testGetAllAuthorsMuid,
      testCase "Test authors cache" testAuthorCache,
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
      testCase "Test Janitor wipe crawler" testJanitorWipeCrawler,
      testCase "Test Janitor update idents" testJanitorUpdateIdents,
      testCase "Test Config Index initialization" testEnsureConfig,
      testCase "Test Config Upgrade to version 1" testUpgradeConfigV1
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
            ( S.AndExpr (S.OrExpr (S.GtExpr "a" "42") (S.EqExpr "a" "0")) (S.EqExpr "b" "d")
            )
        ),
      testCase
        "Parser aliases"
        ( parseMatch'
            [("sprint42", S.GtExpr "date" "2021-07-01")]
            "status:open sprint42"
            ( S.AndExpr (S.EqExpr "status" "open") (S.GtExpr "date" "2021-07-01")
            )
        ),
      testCase
        "Parser implicit and"
        ( parseMatch
            "state:open author:foo"
            ( S.AndExpr (S.EqExpr "state" "open") (S.EqExpr "author" "foo")
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
            "task.score>200"
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
        "Query tag field"
        ( queryMatch
            "tag:bug"
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
            "task.score:1 (task.score:2 or task.score:3 or task.score:4) task.score:5 task.score:6"
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
        ((P.parse aliases code >>= Q.queryWithMods now mempty testTenant) <&> field)
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
            Just
              [ let name = "sprint42"
                    alias = "from:2021-01-01 to:2021-01-21"
                 in Config.SearchAlias {..}
              ],
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
    [ testConfigLoad,
      testGetTenantGroups,
      testGetIdentByAlias
    ]
  where
    createIdent :: Text -> [Text] -> [Text] -> Config.Ident
    createIdent ident aliases groups' =
      let groups = Just groups' in Config.Ident {..}

    testConfigLoad = testCase "Decode config" $ do
      conf <- Config.loadConfig "./test/data/config.yaml"
      assertEqual "config is loaded" 1 (length $ conf & Config.workspaces)

    testGetTenantGroups = testCase "Validate getTenantGroups" $ do
      let identA = createIdent "alice" [] ["core", "ptl"]
          identB = createIdent "bob" [] ["core"]
          tenant = (Config.mkTenant "test") {Config.idents = Just [identA, identB]}
      assertEqual
        "Ensure groups and members"
        [("core", ["bob", "alice"]), ("ptl", ["alice"])]
        (Config.getTenantGroups tenant)

    testGetIdentByAlias = testCase "Validate getIdentByAliases" $ do
      let identA = createIdent "alice" ["opendev.org/Alice Doe/12345", "github.com/alice89"] []
          identB = createIdent "bob" [] []
          tenant = (Config.mkTenant "test") {Config.idents = Just [identA, identB]}
      assertEqual
        "Ensure found alice as ident"
        (Just "alice")
        $ Config.getIdentByAlias tenant "github.com/alice89"
      assertEqual
        "Ensure found no ident"
        Nothing
        $ Config.getIdentByAlias tenant "github.com/ghost"
