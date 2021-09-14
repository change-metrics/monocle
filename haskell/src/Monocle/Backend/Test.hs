-- |
module Monocle.Backend.Test where

import Control.Exception (bracket_)
import Control.Monad.Random.Lazy
import Data.List (partition)
import qualified Data.Text as Text
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import qualified Google.Protobuf.Timestamp as TS
import qualified Monocle.Api.Config as Config
import Monocle.Backend.Documents
import qualified Monocle.Backend.Index as I
import qualified Monocle.Backend.Queries as Q
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Env
import Monocle.Prelude
import qualified Monocle.Search as SearchPB
import Monocle.Search.Query (defaultQueryFlavor)
import qualified Monocle.Search.Query as Q
import Monocle.TaskData
import Relude.Unsafe ((!!))

fakeDate :: UTCTime
fakeDate = fromMaybe (error "nop") (readMaybe "2021-05-31 10:00:00 Z")

fakeDateAlt :: UTCTime
fakeDateAlt = fromMaybe (error "nop") (readMaybe "2021-06-01 20:00:00 Z")

fakeAuthor :: Author
fakeAuthor = Author "John" "John"

mkFakeChange :: Int -> LText -> ELKChange
mkFakeChange number title =
  fakeChange
    { elkchangeId = "aFakeId-" <> show number,
      elkchangeNumber = number,
      elkchangeTitle = title,
      elkchangeUrl = "https://fakehost/change/" <> show number
    }

fakeChange :: ELKChange
fakeChange =
  ELKChange
    { elkchangeId = "",
      elkchangeType = ElkChange,
      elkchangeNumber = 1,
      elkchangeChangeId = "change-id",
      elkchangeTitle = "",
      elkchangeUrl = "",
      elkchangeCommitCount = 1,
      elkchangeAdditions = 1,
      elkchangeDeletions = 1,
      elkchangeChangedFilesCount = 1,
      elkchangeChangedFiles = [File 0 0 "/fake/path"],
      elkchangeText = "",
      elkchangeCommits = [],
      elkchangeRepositoryPrefix = "",
      elkchangeRepositoryFullname = "",
      elkchangeRepositoryShortname = "",
      elkchangeAuthor = fakeAuthor,
      elkchangeBranch = "",
      elkchangeCreatedAt = fakeDate,
      elkchangeUpdatedAt = fakeDate,
      elkchangeMergedBy = Nothing,
      elkchangeTargetBranch = "main",
      elkchangeMergedAt = Nothing,
      elkchangeClosedAt = Nothing,
      elkchangeDuration = Nothing,
      elkchangeApproval = Just ["OK"],
      elkchangeSelfMerged = Nothing,
      elkchangeTasksData = Nothing,
      elkchangeState = ElkChangeOpen,
      elkchangeMergeable = "",
      elkchangeLabels = [],
      elkchangeAssignees = [],
      elkchangeDraft = False
    }

withTenant :: TenantM () -> IO ()
withTenant cb = bracket_ create delete run
  where
    -- todo: generate random name
    testName = "test-tenant"
    config = Config.defaultTenant testName
    create = testTenantM config I.ensureIndex
    delete = testTenantM config I.removeIndex
    run = testTenantM config cb

checkELKChangeField :: (Show a, Eq a) => BH.DocId -> (ELKChange -> a) -> a -> TenantM ()
checkELKChangeField docId field value = do
  docM <- I.getDocumentById docId
  case docM of
    Just change -> assertEqual' "change field match" (field change) value
    Nothing -> error "Change not found"

checkChangesCount :: Int -> TenantM ()
checkChangesCount expectedCount = do
  index <- getIndexName
  resp <-
    BH.countByIndex
      index
      ( BH.CountQuery (BH.TermQuery (BH.Term "type" "Change") Nothing)
      )
  case resp of
    Left _ -> error "Couldn't count changes"
    Right countD -> assertEqual' "check change count" expectedCount (fromEnum $ BH.crCount countD)

testIndexChanges :: Assertion
testIndexChanges = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Index two Changes and check present in database
      I.indexChanges [fakeChange1, fakeChange2]
      checkDocExists' $ I.getChangeDocId fakeChange1
      checkELKChangeField
        (I.getChangeDocId fakeChange1)
        elkchangeTitle
        (elkchangeTitle fakeChange1)
      checkDocExists' $ I.getChangeDocId fakeChange2
      checkELKChangeField
        (I.getChangeDocId fakeChange2)
        elkchangeTitle
        (elkchangeTitle fakeChange2)
      -- Update a Change and ensure the document is updated in the database
      I.indexChanges [fakeChange1Updated]
      checkDocExists' $ I.getChangeDocId fakeChange1
      checkELKChangeField
        (I.getChangeDocId fakeChange1Updated)
        elkchangeTitle
        (elkchangeTitle fakeChange1Updated)
      -- Check total count of Change document in the database
      checkChangesCount 2
      where
        checkDocExists' dId = do
          exists <- I.checkDocExists dId
          assertEqual' "check doc exists" exists True
        fakeChange1 = mkFakeChange 1 "My change 1"
        fakeChange1Updated = fakeChange1 {elkchangeTitle = "My change 1 updated"}
        fakeChange2 = mkFakeChange 2 "My change 2"

-- | A lifted version of assertEqual
assertEqual' :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual' n a b = liftIO $ assertEqual n a b

testProjectCrawlerMetadata :: Assertion
testProjectCrawlerMetadata = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Init default crawler metadata and Ensure we get the default updated date
      I.initCrawlerMetadata worker
      lastUpdated <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" fakeDefaultDate $ snd lastUpdated

      -- Update some crawler metadata and ensure we get the oldest (name, last_commit_at)
      I.setLastUpdated crawlerName fakeDateB entity
      I.setLastUpdated crawlerName fakeDateA entityAlt
      lastUpdated' <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" ("centos/nova", fakeDateB) lastUpdated'

      -- Update one crawler and ensure we get the right oldest
      I.setLastUpdated crawlerName fakeDateC entity
      lastUpdated'' <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" ("centos/neutron", fakeDateA) lastUpdated''

      -- Re run init and ensure it was noop
      I.initCrawlerMetadata worker
      lastUpdated''' <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" ("centos/neutron", fakeDateA) lastUpdated'''
      where
        entityType = CrawlerPB.CommitInfoRequest_EntityTypeProject
        entity = Project "centos/nova"
        entityAlt = Project "centos/neutron"
        crawlerName = "test-crawler"
        worker =
          let name = crawlerName
              update_since = toText fakeDefaultDateStr
              provider =
                let gitlab_url = Just "https://localhost"
                    gitlab_token = Nothing
                    gitlab_repositories = Just ["nova", "neutron"]
                    gitlab_organization = "centos"
                 in Config.GitlabProvider Config.Gitlab {..}
           in Config.Crawler {..}
        fakeDefaultDateStr = "2020-01-01 00:00:00 Z"
        fakeDefaultDate = fromMaybe (error "nop") (readMaybe fakeDefaultDateStr :: Maybe UTCTime)
        fakeDateB = fromMaybe (error "nop") (readMaybe "2021-05-31 10:00:00 Z" :: Maybe UTCTime)
        fakeDateA = fromMaybe (error "nop") (readMaybe "2021-06-01 20:00:00 Z" :: Maybe UTCTime)
        fakeDateC = fromMaybe (error "nop") (readMaybe "2021-06-02 23:00:00 Z" :: Maybe UTCTime)

testOrganizationCrawlerMetadata :: Assertion
testOrganizationCrawlerMetadata = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Init crawler entities metadata and check we get the default date
      I.initCrawlerMetadata worker
      lastUpdated <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" fakeDefaultDate $ snd lastUpdated

      -- TODO(fbo) extract Server.AddProjects and use it directly
      I.initCrawlerEntities (Project <$> ["nova", "neutron"]) worker
      projectA <- I.checkDocExists $ getProjectCrawlerDocId "nova"
      projectB <- I.checkDocExists $ getProjectCrawlerDocId "neutron"
      assertEqual' "Check crawler metadata for projectA present" True projectA
      assertEqual' "Check crawler metadata for projectB present" True projectB

      -- Update the crawler metadata
      I.setLastUpdated crawlerName fakeDateA $ Organization "gitlab-org"
      lastUpdated' <- I.getLastUpdated worker entityType 0
      assertEqual' "check got oldest updated entity" ("gitlab-org", fakeDateA) lastUpdated'
      where
        entityType = CrawlerPB.CommitInfoRequest_EntityTypeOrganization
        fakeDefaultDateStr = "2020-01-01 00:00:00 Z"
        fakeDefaultDate = fromMaybe (error "nop") (readMaybe fakeDefaultDateStr :: Maybe UTCTime)
        fakeDateA = fromMaybe (error "nop") (readMaybe "2021-06-01 20:00:00 Z" :: Maybe UTCTime)
        crawlerName = "test-crawler"
        getProjectCrawlerDocId =
          I.getCrawlerMetadataDocId
            crawlerName
            ( I.getCrawlerTypeAsText CrawlerPB.CommitInfoRequest_EntityTypeProject
            )
        worker =
          let name = crawlerName
              update_since = toText fakeDefaultDateStr
              provider =
                let gitlab_url = Just "https://localhost"
                    gitlab_token = Nothing
                    gitlab_repositories = Nothing
                    gitlab_organization = "gitlab-org"
                 in Config.GitlabProvider Config.Gitlab {..}
           in Config.Crawler {..}

alice :: Author
alice = Author "alice" "a"

bob :: Author
bob = Author "bob" "b"

eve :: Author
eve = Author "eve" "e"

reviewers :: [Author]
reviewers = [alice, bob]

scenarioProject :: LText -> ScenarioProject
scenarioProject name =
  SProject name reviewers [alice] [eve]

testAchievements :: Assertion
testAchievements = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      indexScenario (nominalMerge (scenarioProject "openstack/nova") "42" fakeDate 3600)

      -- Try query
      agg <- head . fromMaybe (error "noagg") . nonEmpty <$> Q.getProjectAgg query
      assertEqual' "event found" (Q.epbType agg) "Change"
      assertEqual' "event count match" (Q.epbCount agg) 1
      where
        query = case (Q.queryGet $ Q.load Nothing mempty Nothing "state:merged") id (Just defaultQueryFlavor) of
          [x] -> x
          _ -> error "Could not compile query"

defaultQuery :: Q.Query
defaultQuery =
  let queryGet _ = const []
      queryBounds =
        ( fromMaybe (error "nop") (readMaybe "2000-01-01 00:00:00 Z"),
          fromMaybe (error "nop") (readMaybe "2099-12-31 23:59:59 Z")
        )
      queryMinBoundsSet = False
   in Q.Query {..}

testReposSummary :: Assertion
testReposSummary = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      indexScenario (nominalMerge (scenarioProject "openstack/nova") "42" fakeDate 3600)
      indexScenario (nominalMerge (scenarioProject "openstack/neutron") "43" fakeDate 3600)
      indexScenario (nominalMerge (scenarioProject "openstack/neutron") "44" fakeDate 3600)

      results <- runQueryM defaultQuery Q.getReposSummary
      assertEqual'
        "Check buckets names"
        [ Q.RepoSummary
            { fullname = "openstack/neutron",
              totalChanges = 2,
              abandonedChanges = 0,
              mergedChanges = 2,
              openChanges = 0
            },
          Q.RepoSummary
            { fullname = "openstack/nova",
              totalChanges = 1,
              abandonedChanges = 0,
              mergedChanges = 1,
              openChanges = 0
            }
        ]
        results

testTopAuthors :: Assertion
testTopAuthors = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Prapare data
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
      traverse_ (indexScenarioNM nova) ["42", "43"]
      traverse_ (indexScenarioNO neutron) ["142", "143"]

      -- Check for expected metrics
      runQueryM defaultQuery $ do
        results <- Q.getMostActiveAuthorByChangeCreated 10
        assertEqual'
          "Check getMostActiveAuthorByChangeCreated count"
          [Q.TermResult {trTerm = "eve", trCount = 4}]
          (Q.tsrTR results)
        results' <- Q.getMostActiveAuthorByChangeMerged 10
        assertEqual'
          "Check getMostActiveAuthorByChangeMerged count"
          [Q.TermResult {trTerm = "eve", trCount = 2}]
          (Q.tsrTR results')
        results'' <- Q.getMostActiveAuthorByChangeReviewed 10
        assertEqual'
          "Check getMostActiveAuthorByChangeReviewed count"
          [ Q.TermResult {trTerm = "alice", trCount = 2},
            Q.TermResult {trTerm = "bob", trCount = 2}
          ]
          (Q.tsrTR results'')
        results''' <- Q.getMostActiveAuthorByChangeCommented 10
        assertEqual'
          "Check getMostActiveAuthorByChangeCommented count"
          [Q.TermResult {trTerm = "alice", trCount = 2}]
          (Q.tsrTR results''')
        results'''' <- Q.getMostReviewedAuthor 10
        assertEqual'
          "Check getMostReviewedAuthor count"
          [Q.TermResult {trTerm = "eve", trCount = 4}]
          (Q.tsrTR results'''')
        results''''' <- Q.getMostCommentedAuthor 10
        assertEqual'
          "Check getMostCommentedAuthor count"
          [Q.TermResult {trTerm = "eve", trCount = 2}]
          (Q.tsrTR results''''')

testGetAuthorsPeersStrength :: Assertion
testGetAuthorsPeersStrength = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Prapare data
      let nova = SProject "openstack/nova" [bob] [alice] [eve]
      let neutron = SProject "openstack/neutron" [alice] [eve] [bob]
      let horizon = SProject "openstack/horizon" [alice] [alice] [alice]
      traverse_ (indexScenarioNM nova) ["42", "43"]
      traverse_ (indexScenarioNM neutron) ["142"]
      traverse_ (indexScenarioNM horizon) ["242"]

      -- Check for expected metrics
      runQueryM defaultQuery $ do
        results <- Q.getAuthorsPeersStrength 10
        assertEqual'
          "Check getAuthorsPeersStrength results"
          [ Q.PeerStrengthResult
              { psrAuthor = "eve",
                psrPeer = "bob",
                psrStrength = 4
              },
            Q.PeerStrengthResult
              { psrAuthor = "bob",
                psrPeer = "alice",
                psrStrength = 2
              }
          ]
          results

testGetNewContributors :: Assertion
testGetNewContributors = withTenant doTest
  where
    indexScenario' project fakeDate' cid = indexScenario (nominalMerge project cid fakeDate' 3600)
    doTest :: TenantM ()
    doTest = do
      -- Prapare data
      let sn1 = SProject "openstack/nova" [bob] [alice] [eve]
      let sn2 = SProject "openstack/nova" [bob] [alice] [bob]

      indexScenario' sn1 fakeDate "42"
      indexScenario' sn1 (addUTCTime 7200 fakeDate) "43"
      indexScenario' sn2 (addUTCTime 7200 fakeDate) "44"

      let query =
            let queryGet _ = const []
                queryBounds =
                  ( addUTCTime 3600 fakeDate,
                    fromMaybe (error "nop") (readMaybe "2099-12-31 23:59:59 Z")
                  )
                queryMinBoundsSet = True
             in Q.Query {..}

      runQueryM query $ do
        results <- Q.getNewContributors
        assertEqual'
          "Check getNewContributors results"
          [Q.TermResult {trTerm = "bob", trCount = 1}]
          results

testGetActivityStats :: Assertion
testGetActivityStats = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      -- Prapare data
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
      traverse_ (indexScenarioNM nova) ["42", "43"]
      traverse_ (indexScenarioNO neutron) ["142", "143"]

      let query =
            let queryGet _ = const []
                queryBounds =
                  ( addUTCTime (-3600) fakeDate,
                    addUTCTime 3600 fakeDate
                  )
                queryMinBoundsSet = True
             in Q.Query {..}

      runQueryM query $ do
        results <- Q.getActivityStats
        assertEqual'
          "Check getActivityStats result"
          ( SearchPB.ActivityStats
              1
              1
              2
              ( V.fromList
                  [ SearchPB.Histo {histoDate = "2021-05-31 09:00", histoCount = 0},
                    SearchPB.Histo {histoDate = "2021-05-31 10:00", histoCount = 1},
                    SearchPB.Histo {histoDate = "2021-05-31 11:00", histoCount = 0}
                  ]
              )
              ( V.fromList
                  [ SearchPB.Histo {histoDate = "2021-05-31 09:00", histoCount = 0},
                    SearchPB.Histo {histoDate = "2021-05-31 10:00", histoCount = 2},
                    SearchPB.Histo {histoDate = "2021-05-31 11:00", histoCount = 0}
                  ]
              )
              ( V.fromList
                  [ SearchPB.Histo {histoDate = "2021-05-31 09:00", histoCount = 0},
                    SearchPB.Histo {histoDate = "2021-05-31 10:00", histoCount = 1},
                    SearchPB.Histo {histoDate = "2021-05-31 11:00", histoCount = 0}
                  ]
              )
          )
          results

testGetChangesTops :: Assertion
testGetChangesTops = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
      traverse_ (indexScenarioNM nova) ["42", "43"]
      traverse_ (indexScenarioNO neutron) ["142", "143"]

      runQueryM defaultQuery $ do
        results <- Q.getChangesTops 10
        assertEqual'
          "Check getChangesTops result"
          ( SearchPB.ChangesTops
              { changesTopsAuthors =
                  Just
                    ( SearchPB.TermsCount
                        { termsCountTermcount =
                            V.fromList
                              [ SearchPB.TermCount
                                  { termCountTerm = "eve",
                                    termCountCount = 4
                                  }
                              ],
                          termsCountTotalHits = 4
                        }
                    ),
                changesTopsRepos =
                  Just
                    ( SearchPB.TermsCount
                        { termsCountTermcount =
                            V.fromList
                              [ SearchPB.TermCount
                                  { termCountTerm = "openstack/neutron",
                                    termCountCount = 2
                                  },
                                SearchPB.TermCount
                                  { termCountTerm = "openstack/nova",
                                    termCountCount = 2
                                  }
                              ],
                          termsCountTotalHits = 4
                        }
                    ),
                changesTopsApprovals =
                  Just
                    ( SearchPB.TermsCount
                        { termsCountTermcount =
                            V.fromList
                              [ SearchPB.TermCount
                                  { termCountTerm = "OK",
                                    termCountCount = 4
                                  }
                              ],
                          termsCountTotalHits = 4
                        }
                    )
              }
          )
          results

testGetSuggestions :: Assertion
testGetSuggestions = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      tEnv <- ask
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      let neutron = SProject "openstack/neutron" [eve] [alice] [bob]
      traverse_ (indexScenarioNM nova) ["42", "43"]
      traverse_ (indexScenarioNO neutron) ["142", "143"]

      runQueryM defaultQuery $ do
        results <- Q.getSuggestions $ tenant tEnv
        assertEqual'
          "Check getChangesTops result"
          ( SearchPB.SuggestionsResponse
              mempty
              (V.fromList ["bob", "eve"])
              (V.fromList ["OK"])
              mempty
              mempty
              mempty
              mempty
          )
          results

testTaskDataAdd :: Assertion
testTaskDataAdd = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      traverse_ (indexScenarioNM nova) ["42", "43", "44"]

      -- Send Task data with a matching changes
      void $ I.taskDataAdd [mkTaskData "42", mkTaskData "43"]
      -- Ensure only changes 42 and 43 got a Task data associated
      changes <- I.getChangesByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"]) 3
      assertEqual'
        "Check adding matching taskData"
        [ ("44", Nothing),
          ( "43",
            Just
              [ ELKTaskData
                  { tdTid = "",
                    tdTtype = [],
                    tdChangeUrl = "https://fakeprovider/43",
                    tdSeverity = "",
                    tdPriority = "",
                    tdScore = 0,
                    tdUrl = "https://tdprovider/42-43",
                    tdTitle = ""
                  }
              ]
          ),
          ( "42",
            Just
              [ ELKTaskData
                  { tdTid = "",
                    tdTtype = [],
                    tdChangeUrl = "https://fakeprovider/42",
                    tdSeverity = "",
                    tdPriority = "",
                    tdScore = 0,
                    tdUrl = "https://tdprovider/42-42",
                    tdTitle = ""
                  }
              ]
          )
        ]
        ((\ELKChange {..} -> (elkchangeId, elkchangeTasksData)) <$> changes)
      -- Ensure associated ChangeEvents got the Task data attibutes
      events <- I.getChangesEventsByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"]) 100
      let (withTD, withoutTD) = partition (isJust . elkchangeeventTasksData) events
          createdEventWithTD =
            filter
              (\e -> (e & elkchangeeventType) == ElkChangeCreatedEvent)
              withTD
      assertEqual' "Check events count that got a Task data" 8 (length withTD)
      assertEqual' "Check events count that miss a Task data" 4 (length withoutTD)
      assertEqual'
        "Check Change events got the task data attribute"
        [ ( "ChangeCreatedEvent-42",
            Just
              [ ELKTaskData
                  { tdTid = "",
                    tdTtype = [],
                    tdChangeUrl = "https://fakeprovider/42",
                    tdSeverity = "",
                    tdPriority = "",
                    tdScore = 0,
                    tdUrl = "https://tdprovider/42-42",
                    tdTitle = ""
                  }
              ]
          ),
          ( "ChangeCreatedEvent-43",
            Just
              [ ELKTaskData
                  { tdTid = "",
                    tdTtype = [],
                    tdChangeUrl = "https://fakeprovider/43",
                    tdSeverity = "",
                    tdPriority = "",
                    tdScore = 0,
                    tdUrl = "https://tdprovider/42-43",
                    tdTitle = ""
                  }
              ]
          )
        ]
        ( ( \ELKChangeEvent {..} ->
              (elkchangeeventId, elkchangeeventTasksData)
          )
            <$> createdEventWithTD
        )

      -- Send a Task data w/o a matching change
      let td = mkTaskData "45"
      void $ I.taskDataAdd [td]
      -- Ensure the Task data has been stored as orphan (we can find it by its url as DocId)
      orphanTdM <- getOrphanTd . toText $ td & taskDataUrl
      assertEqual'
        "Check Task data stored as Orphan Task Data"
        ( Just
            ( ELKChangeOrphanTD
                { elkchangeorphantdType = ElkOrphanTaskData,
                  elkchangeorphantdTasksData =
                    Just
                      [ ELKTaskData
                          { tdTid = "",
                            tdTtype = [],
                            tdChangeUrl = "https://fakeprovider/45",
                            tdSeverity = "",
                            tdPriority = "",
                            tdScore = 0,
                            tdUrl = "https://tdprovider/42-45",
                            tdTitle = ""
                          }
                      ]
                }
            )
        )
        orphanTdM

    mkTaskData changeId =
      let taskDataUpdatedAt = Just $ TS.fromUTCTime fakeDate
          taskDataChangeUrl = "https://fakeprovider/" <> changeId
          taskDataTtype = mempty
          taskDataTid = ""
          taskDataUrl = "https://tdprovider/42-" <> changeId
          taskDataTitle = ""
          taskDataSeverity = ""
          taskDataPriority = ""
          taskDataScore = 0
       in TaskData {..}
    getOrphanTd :: Text -> TenantM (Maybe ELKChangeOrphanTD)
    getOrphanTd url = I.getDocumentById $ BH.DocId $ I.getBase64Text url

testTaskDataCommit :: Assertion
testTaskDataCommit = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      let crawlerName = "testCrawler"
          crawlerConfig =
            let name = crawlerName
                provider = Config.TaskDataProvider
                update_since = "2020-01-01"
             in Config.Crawler {..}
      -- Test get default commit date from config (as no previous crawler metadata)
      commitDate <- I.getTDCrawlerCommitDate crawlerName crawlerConfig
      assertEqual'
        "Task data crawler metadata - check default date"
        ( fromMaybe (error "nop") (readMaybe "2020-01-01 00:00:00 Z")
        )
        commitDate
      -- Test that we can commit a date and make sure we get it back
      void $ I.setTDCrawlerCommitDate crawlerName fakeDate
      commitDate' <- I.getTDCrawlerCommitDate crawlerName crawlerConfig
      assertEqual'
        "Task data crawler metadata - check commited date "
        fakeDate
        commitDate'
      -- Test that we can update the commit and that we get it back
      void $ I.setTDCrawlerCommitDate crawlerName fakeDateAlt
      commitDate'' <- I.getTDCrawlerCommitDate crawlerName crawlerConfig
      assertEqual'
        "Task data crawler metadata - check updated commited date "
        fakeDateAlt
        commitDate''

-- Tests scenario helpers

-- $setup
-- >>> import Data.Time.Clock
-- >>> let now = fromMaybe (error "") (readMaybe "2021-06-10 01:21:03Z")

-- | 'randomAuthor' returns a random element of the given list
randomAuthor :: (MonadRandom m) => [a] -> m a
randomAuthor xs = do
  let n = length xs
  i <- getRandomR (0, n -1)
  return (xs !! i)

emptyChange :: ELKChange
emptyChange = fakeChange

emptyEvent :: ELKChangeEvent
emptyEvent = ELKChangeEvent {..}
  where
    elkchangeeventId = mempty
    elkchangeeventNumber = 0
    elkchangeeventChangeId = mempty
    elkchangeeventType = ElkChangeCreatedEvent
    elkchangeeventUrl = mempty
    elkchangeeventChangedFiles = mempty
    elkchangeeventRepositoryPrefix = mempty
    elkchangeeventRepositoryShortname = mempty
    elkchangeeventRepositoryFullname = mempty
    elkchangeeventAuthor = fakeAuthor
    elkchangeeventOnAuthor = fakeAuthor
    elkchangeeventBranch = mempty
    elkchangeeventCreatedAt = fakeDate
    elkchangeeventOnCreatedAt = fakeDate
    elkchangeeventApproval = Nothing
    elkchangeeventTasksData = Nothing

showEvents :: [ScenarioEvent] -> Text
showEvents xs = Text.intercalate ", " $ sort (map go xs)
  where
    author = toStrict . authorMuid
    date = toText . formatTime defaultTimeLocale "%Y-%m-%d"
    go ev = case ev of
      SChange ELKChange {..} -> "Change[" <> toStrict elkchangeChangeId <> "]"
      SCreation ELKChangeEvent {..} ->
        ("Change[" <> date elkchangeeventOnCreatedAt <> " ")
          <> (toStrict elkchangeeventChangeId <> " created by " <> author elkchangeeventAuthor)
          <> "]"
      SComment ELKChangeEvent {..} -> "Commented[" <> author elkchangeeventAuthor <> "]"
      SReview ELKChangeEvent {..} -> "Reviewed[" <> author elkchangeeventAuthor <> "]"
      SMerge ELKChangeEvent {..} -> "Merged[" <> date elkchangeeventOnCreatedAt <> "]"

-- Tests scenario data types

-- | 'ScenarioProject' is a data type to define a project for a scenario.
data ScenarioProject = SProject
  { name :: LText,
    maintainers :: [Author],
    commenters :: [Author],
    contributors :: [Author]
  }

-- | 'ScenarioEvent' is a type of event generated for a given scenario.
data ScenarioEvent
  = SChange ELKChange
  | SCreation ELKChangeEvent
  | SReview ELKChangeEvent
  | SComment ELKChangeEvent
  | SMerge ELKChangeEvent

indexScenario :: [ScenarioEvent] -> TenantM ()
indexScenario xs = sequence_ $ indexDoc <$> xs
  where
    indexDoc = \case
      SChange d -> I.indexChanges [d]
      SCreation d -> I.indexEvents [d]
      SReview d -> I.indexEvents [d]
      SComment d -> I.indexEvents [d]
      SMerge d -> I.indexEvents [d]

indexScenarioNM :: ScenarioProject -> LText -> TenantM ()
indexScenarioNM project cid = indexScenario (nominalMerge project cid fakeDate 3600)

indexScenarioNO :: ScenarioProject -> LText -> TenantM ()
indexScenarioNO project cid = indexScenario (nominalOpen project cid fakeDate 3600)

mkDate :: Integer -> UTCTime -> UTCTime
mkDate elapsed = addUTCTime (secondsToNominalDiffTime (fromInteger elapsed))

mkChange ::
  -- Delta related to start time
  Integer ->
  -- Start time
  UTCTime ->
  -- Change Author
  Author ->
  -- Provider change ID
  LText ->
  -- Repository fullname
  LText ->
  -- Change State
  ELKChangeState ->
  ELKChange
mkChange ts start author changeId name state' =
  emptyChange
    { elkchangeType = ElkChange,
      elkchangeId = changeId,
      elkchangeState = state',
      elkchangeRepositoryFullname = name,
      elkchangeCreatedAt = mkDate ts start,
      elkchangeAuthor = author,
      elkchangeChangeId = "change-" <> changeId,
      elkchangeUrl = "https://fakeprovider/" <> changeId
    }

mkEvent ::
  -- Delta related to start time
  Integer ->
  -- Start time
  UTCTime ->
  -- Type of Event
  ELKDocType ->
  -- Author of the event
  Author ->
  -- Author of the related change
  Author ->
  -- Provider change ID of the related change
  LText ->
  -- Repository fullname
  LText ->
  ELKChangeEvent
mkEvent ts start etype author onAuthor changeId name =
  emptyEvent
    { elkchangeeventAuthor = author,
      elkchangeeventOnAuthor = onAuthor,
      elkchangeeventType = etype,
      elkchangeeventRepositoryFullname = name,
      elkchangeeventId = docTypeToText etype <> "-" <> changeId,
      elkchangeeventCreatedAt = mkDate ts start,
      elkchangeeventOnCreatedAt = mkDate ts start,
      elkchangeeventChangeId = "change-" <> changeId,
      elkchangeeventUrl = "https://fakeprovider/" <> changeId
    }

-- | 'nominalMerge' is the most simple scenario
-- >>> let project = SProject "openstack/nova" [alice, bob] [alice] [eve]
-- >>> showEvents $ nominalMerge project "42" now (3600*24)
-- "Change[2021-06-10 change-42 created by eve], Change[change-42], Commented[alice], Merged[2021-06-11], Reviewed[alice]"
nominalMerge :: ScenarioProject -> LText -> UTCTime -> Integer -> [ScenarioEvent]
nominalMerge SProject {..} changeId start duration = evalRand scenario stdGen
  where
    -- The random number generator is based on the name
    stdGen = mkStdGen (Text.length (toStrict name))

    scenario = do
      -- The base change
      let mkChange' ts author = mkChange ts start author changeId name ElkChangeMerged
          mkEvent' ts etype author onAuthor = mkEvent ts start etype author onAuthor changeId name

      -- The change creation
      author <- randomAuthor contributors
      let create = mkEvent' 0 ElkChangeCreatedEvent author author
          change = mkChange' 0 author

      -- The comment
      commenter <- randomAuthor $ maintainers <> commenters
      let comment = mkEvent' (duration `div` 2) ElkChangeCommentedEvent commenter author

      -- The review
      reviewer <- randomAuthor maintainers
      let review = mkEvent' (duration `div` 2) ElkChangeReviewedEvent reviewer author

      -- The change merged event
      approver <- randomAuthor maintainers
      let merge = mkEvent' duration ElkChangeMergedEvent approver author

      -- The event lists
      pure [SChange change, SCreation create, SComment comment, SReview review, SMerge merge]

nominalOpen :: ScenarioProject -> LText -> UTCTime -> Integer -> [ScenarioEvent]
nominalOpen SProject {..} changeId start duration = evalRand scenario stdGen
  where
    -- The random number generator is based on the name
    stdGen = mkStdGen (Text.length (toStrict name))

    scenario = do
      -- The base change
      let mkChange' ts author = mkChange ts start author changeId name ElkChangeOpen
          mkEvent' ts etype author onAuthor = mkEvent ts start etype author onAuthor changeId name

      -- The change creation
      author <- randomAuthor contributors
      let create = mkEvent' 0 ElkChangeCreatedEvent author author
          change = mkChange' 0 author

      -- The review
      reviewer <- randomAuthor maintainers
      let review = mkEvent' (duration `div` 2) ElkChangeReviewedEvent reviewer author

      -- The event lists
      pure [SChange change, SCreation create, SReview review]
