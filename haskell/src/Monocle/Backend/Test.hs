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
import qualified Google.Protobuf.Timestamp as T
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
fakeDate = [utctime|2021-05-31 10:00:00|]

fakeDateAlt :: UTCTime
fakeDateAlt = [utctime|2021-06-01 20:00:00|]

fakeAuthor :: Author
fakeAuthor = Author "John" "John"

mkFakeChange :: Int -> LText -> EChange
mkFakeChange number title =
  fakeChange
    { echangeId = "aFakeId-" <> show number,
      echangeNumber = number,
      echangeTitle = title,
      echangeUrl = "https://fakehost/change/" <> show number
    }

fakeChange :: EChange
fakeChange =
  EChange
    { echangeId = "",
      echangeType = EChangeDoc,
      echangeNumber = 1,
      echangeChangeId = "change-id",
      echangeTitle = "",
      echangeUrl = "",
      echangeCommitCount = 1,
      echangeAdditions = 1,
      echangeDeletions = 1,
      echangeChangedFilesCount = 1,
      echangeChangedFiles = [File 0 0 "/fake/path"],
      echangeText = "",
      echangeCommits = [],
      echangeRepositoryPrefix = "",
      echangeRepositoryFullname = "",
      echangeRepositoryShortname = "",
      echangeAuthor = fakeAuthor,
      echangeBranch = "",
      echangeCreatedAt = fakeDate,
      echangeUpdatedAt = fakeDate,
      echangeMergedBy = Nothing,
      echangeTargetBranch = "main",
      echangeMergedAt = Nothing,
      echangeClosedAt = Nothing,
      echangeDuration = Nothing,
      echangeApproval = Just ["OK"],
      echangeSelfMerged = Nothing,
      echangeTasksData = Nothing,
      echangeState = EChangeOpen,
      echangeMergeable = "",
      echangeLabels = [],
      echangeAssignees = [],
      echangeDraft = False
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

checkEChangeField :: (Show a, Eq a) => BH.DocId -> (EChange -> a) -> a -> TenantM ()
checkEChangeField docId field value = do
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
      checkEChangeField
        (I.getChangeDocId fakeChange1)
        echangeTitle
        (echangeTitle fakeChange1)
      checkDocExists' $ I.getChangeDocId fakeChange2
      checkEChangeField
        (I.getChangeDocId fakeChange2)
        echangeTitle
        (echangeTitle fakeChange2)
      -- Update a Change and ensure the document is updated in the database
      I.indexChanges [fakeChange1Updated]
      checkDocExists' $ I.getChangeDocId fakeChange1
      checkEChangeField
        (I.getChangeDocId fakeChange1Updated)
        echangeTitle
        (echangeTitle fakeChange1Updated)
      -- Check total count of Change document in the database
      checkChangesCount 2
      where
        checkDocExists' dId = do
          exists <- I.checkDocExists dId
          assertEqual' "check doc exists" exists True
        fakeChange1 = mkFakeChange 1 "My change 1"
        fakeChange1Updated = fakeChange1 {echangeTitle = "My change 1 updated"}
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
              update_since = show fakeDefaultDate
              provider =
                let gitlab_url = Just "https://localhost"
                    gitlab_token = Nothing
                    gitlab_repositories = Just ["nova", "neutron"]
                    gitlab_organization = "centos"
                 in Config.GitlabProvider Config.Gitlab {..}
           in Config.Crawler {..}
        fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
        fakeDateB = [utctime|2021-05-31 10:00:00|]
        fakeDateA = [utctime|2021-06-01 20:00:00|]
        fakeDateC = [utctime|2021-06-02 23:00:00|]

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
        fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
        fakeDateA = [utctime|2021-06-01 20:00:00|]
        crawlerName = "test-crawler"
        getProjectCrawlerDocId =
          I.getCrawlerMetadataDocId
            crawlerName
            ( I.getCrawlerTypeAsText CrawlerPB.CommitInfoRequest_EntityTypeProject
            )
        worker =
          let name = crawlerName
              update_since = show fakeDefaultDate
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
        ( [utctime|2000-01-01 00:00:00|],
          [utctime|2099-12-31 23:59:59|]
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
      indexScenario (nominalOpen (scenarioProject "openstack/swift") "45" fakeDate 3600)

      results <- runQueryM defaultQuery Q.getReposSummary
      assertEqual'
        "Check buckets names"
        [ Q.RepoSummary
            { fullname = "openstack/neutron",
              createdChanges = 2,
              abandonedChanges = 0,
              mergedChanges = 2,
              updatedChanges = 0,
              openChanges = 0
            },
          Q.RepoSummary
            { fullname = "openstack/nova",
              createdChanges = 1,
              abandonedChanges = 0,
              mergedChanges = 1,
              updatedChanges = 0,
              openChanges = 0
            },
          Q.RepoSummary
            { fullname = "openstack/swift",
              createdChanges = 1,
              abandonedChanges = 0,
              mergedChanges = 0,
              updatedChanges = 1,
              openChanges = 1
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
                    [utctime|2099-12-31 23:59:59|]
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

mkTaskData :: LText -> TaskData
mkTaskData changeId =
  let taskDataUpdatedAt = Just $ T.fromUTCTime fakeDate
      taskDataChangeUrl = "https://fakeprovider/" <> changeId
      taskDataTtype = mempty
      taskDataTid = ""
      taskDataUrl = "https://tdprovider/42-" <> changeId
      taskDataTitle = ""
      taskDataSeverity = ""
      taskDataPriority = ""
      taskDataScore = 0
      taskDataPrefix = "lada"
   in TaskData {..}

testTaskDataAdd :: Assertion
testTaskDataAdd = withTenant doTest
  where
    doTest :: TenantM ()
    doTest = do
      let nova = SProject "openstack/nova" [alice] [alice] [eve]
      traverse_ (indexScenarioNM nova) ["42", "43", "44"]

      -- Send Task data with a matching changes
      let td42 = mkTaskData "42"
          td43 = mkTaskData "43"
      void $ I.taskDataAdd [td42, td43]
      -- Ensure only changes 42 and 43 got a Task data associated
      changes <- I.getChangesByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"])
      assertEqual'
        "Check adding matching taskData"
        [ ("42", Just [I.toETaskData td42]),
          ("43", Just [I.toETaskData td43]),
          ("44", Nothing)
        ]
        ((\EChange {..} -> (echangeId, echangeTasksData)) <$> changes)
      -- Ensure associated ChangeEvents got the Task data attibutes
      events <- I.getChangesEventsByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"])
      let (withTD, withoutTD) = partition (isJust . echangeeventTasksData) events
          createdEventWithTD =
            filter
              (\e -> (e & echangeeventType) == EChangeCreatedEvent)
              withTD
      assertEqual' "Check events count that got a Task data" 8 (length withTD)
      assertEqual' "Check events count that miss a Task data" 4 (length withoutTD)
      assertEqual'
        "Check Change events got the task data attribute"
        [ ("ChangeCreatedEvent-42", Just [I.toETaskData td42]),
          ("ChangeCreatedEvent-43", Just [I.toETaskData td43])
        ]
        ( ( \EChangeEvent {..} ->
              (echangeeventId, echangeeventTasksData)
          )
            <$> createdEventWithTD
        )

      -- Send a Task data w/o a matching change (orphan task data)
      let td = mkTaskData "45"
      void $ I.taskDataAdd [td]
      -- Ensure the Task data has been stored as orphan (we can find it by its url as DocId)
      orphanTdM <- getOrphanTd . toText $ td & taskDataUrl
      let expectedTD = I.toETaskData td
      assertEqual'
        "Check Task data stored as Orphan Task Data"
        ( Just
            ( EChangeOrphanTD
                { echangeorphantdId = I.getBase64Text "https://tdprovider/42-45",
                  echangeorphantdType = EOrphanTaskData,
                  echangeorphantdTasksData = expectedTD
                }
            )
        )
        orphanTdM

      -- Send the same orphan task data with an updated field and ensure it has been
      -- updated in the Database
      let td' = td {taskDataSeverity = "urgent"}
      void $ I.taskDataAdd [td']
      orphanTdM' <- getOrphanTd . toText $ td' & taskDataUrl
      let expectedTD' = expectedTD {tdSeverity = "urgent"}
      assertEqual'
        "Check Task data stored as Orphan Task Data"
        ( Just
            ( EChangeOrphanTD
                { echangeorphantdId = I.getBase64Text "https://tdprovider/42-45",
                  echangeorphantdType = EOrphanTaskData,
                  echangeorphantdTasksData = expectedTD'
                }
            )
        )
        orphanTdM'

    getOrphanTd :: Text -> TenantM (Maybe EChangeOrphanTD)
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
        [utctime|2020-01-01 00:00:00|]
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

testTaskDataAdoption :: Assertion
testTaskDataAdoption = withTenant doTest
  where
    doTest :: TenantM ()
    doTest =
      do
        -- Send Task data w/o a matching change (orphan task data)
        let td42 = mkTaskData "42"
            td43 = mkTaskData "43"
        void $ I.taskDataAdd [td42, td43]
        oTDs <- I.getOrphanTaskDataByChangeURL $ toText . taskDataChangeUrl <$> [td42, td43]
        assertEqual' "Check we can fetch the orphan task data" 2 (length oTDs)

        -- Index a change and related events
        let scenario = nominalMerge (SProject "openstack/nova" [alice, bob] [alice] [eve]) "42" fakeDate 3600
            events = catMaybes $ getScenarioEvtObj <$> scenario
            changes = catMaybes $ getScenarioChangeObj <$> scenario
        indexScenario scenario
        I.updateChangesAndEventsFromOrphanTaskData changes events
        -- Check that the matching task data has been adopted
        oTDs' <- I.getOrphanTaskDataByChangeURL $ toText . taskDataChangeUrl <$> [td42, td43]
        assertEqual' "Check remaining one orphan TD" 1 (length oTDs')
        -- Check that change and related events got the task data attribute
        changes' <- I.getChangesByURL [changeUrl]
        events' <- I.getChangesEventsByURL [changeUrl]
        let haveTDs =
              all
                (== True)
                $ (isJust . echangeTasksData <$> changes')
                  <> (isJust . echangeeventTasksData <$> events')
        assertEqual' "Check objects related to change 42 got the Tasks data" True haveTDs
      where
        getScenarioEvtObj :: ScenarioEvent -> Maybe EChangeEvent
        getScenarioEvtObj (SCreation obj) = Just obj
        getScenarioEvtObj (SComment obj) = Just obj
        getScenarioEvtObj (SReview obj) = Just obj
        getScenarioEvtObj (SMerge obj) = Just obj
        getScenarioEvtObj _ = Nothing
        getScenarioChangeObj (SChange obj) = Just obj
        getScenarioChangeObj _ = Nothing
        changeUrl = "https://fakeprovider/42"

-- Tests scenario helpers

-- $setup
-- >>> import Data.Time.Clock
-- >>> let now = [utctime|2021-06-10 01:21:03|]

-- | 'randomAuthor' returns a random element of the given list
randomAuthor :: (MonadRandom m) => [a] -> m a
randomAuthor xs = do
  let n = length xs
  i <- getRandomR (0, n -1)
  return (xs !! i)

emptyChange :: EChange
emptyChange = fakeChange

emptyEvent :: EChangeEvent
emptyEvent = EChangeEvent {..}
  where
    echangeeventId = mempty
    echangeeventNumber = 0
    echangeeventChangeId = mempty
    echangeeventType = EChangeCreatedEvent
    echangeeventUrl = mempty
    echangeeventChangedFiles = mempty
    echangeeventRepositoryPrefix = mempty
    echangeeventRepositoryShortname = mempty
    echangeeventRepositoryFullname = mempty
    echangeeventAuthor = Just fakeAuthor
    echangeeventOnAuthor = fakeAuthor
    echangeeventBranch = mempty
    echangeeventCreatedAt = fakeDate
    echangeeventOnCreatedAt = fakeDate
    echangeeventApproval = Nothing
    echangeeventTasksData = Nothing

showEvents :: [ScenarioEvent] -> Text
showEvents xs = Text.intercalate ", " $ sort (map go xs)
  where
    author = maybe "no-author" (toStrict . authorMuid)
    date = toText . formatTime defaultTimeLocale "%Y-%m-%d"
    go ev = case ev of
      SChange EChange {..} -> "Change[" <> toStrict echangeChangeId <> "]"
      SCreation EChangeEvent {..} ->
        ("Change[" <> date echangeeventOnCreatedAt <> " ")
          <> (toStrict echangeeventChangeId <> " created by " <> author echangeeventAuthor)
          <> "]"
      SComment EChangeEvent {..} -> "Commented[" <> author echangeeventAuthor <> "]"
      SReview EChangeEvent {..} -> "Reviewed[" <> author echangeeventAuthor <> "]"
      SMerge EChangeEvent {..} -> "Merged[" <> date echangeeventOnCreatedAt <> "]"

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
  = SChange EChange
  | SCreation EChangeEvent
  | SReview EChangeEvent
  | SComment EChangeEvent
  | SMerge EChangeEvent

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
  EChangeState ->
  EChange
mkChange ts start author changeId name state' =
  emptyChange
    { echangeType = EChangeDoc,
      echangeId = changeId,
      echangeState = state',
      echangeRepositoryFullname = name,
      echangeCreatedAt = mkDate ts start,
      echangeAuthor = author,
      echangeChangeId = "change-" <> changeId,
      echangeUrl = "https://fakeprovider/" <> changeId
    }

mkEvent ::
  -- Delta related to start time
  Integer ->
  -- Start time
  UTCTime ->
  -- Type of Event
  EDocType ->
  -- Author of the event
  Author ->
  -- Author of the related change
  Author ->
  -- Provider change ID of the related change
  LText ->
  -- Repository fullname
  LText ->
  EChangeEvent
mkEvent ts start etype author onAuthor changeId name =
  emptyEvent
    { echangeeventAuthor = Just author,
      echangeeventOnAuthor = onAuthor,
      echangeeventType = etype,
      echangeeventRepositoryFullname = name,
      echangeeventId = docTypeToText etype <> "-" <> changeId,
      echangeeventCreatedAt = mkDate ts start,
      echangeeventOnCreatedAt = mkDate ts start,
      echangeeventChangeId = "change-" <> changeId,
      echangeeventUrl = "https://fakeprovider/" <> changeId
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
      let mkChange' ts author = mkChange ts start author changeId name EChangeMerged
          mkEvent' ts etype author onAuthor = mkEvent ts start etype author onAuthor changeId name

      -- The change creation
      author <- randomAuthor contributors
      let create = mkEvent' 0 EChangeCreatedEvent author author
          change = mkChange' 0 author

      -- The comment
      commenter <- randomAuthor $ maintainers <> commenters
      let comment = mkEvent' (duration `div` 2) EChangeCommentedEvent commenter author

      -- The review
      reviewer <- randomAuthor maintainers
      let review = mkEvent' (duration `div` 2) EChangeReviewedEvent reviewer author

      -- The change merged event
      approver <- randomAuthor maintainers
      let merge = mkEvent' duration EChangeMergedEvent approver author

      -- The event lists
      pure [SChange change, SCreation create, SComment comment, SReview review, SMerge merge]

nominalOpen :: ScenarioProject -> LText -> UTCTime -> Integer -> [ScenarioEvent]
nominalOpen SProject {..} changeId start duration = evalRand scenario stdGen
  where
    -- The random number generator is based on the name
    stdGen = mkStdGen (Text.length (toStrict name))

    scenario = do
      -- The base change
      let mkChange' ts author = mkChange ts start author changeId name EChangeOpen
          mkEvent' ts etype author onAuthor = mkEvent ts start etype author onAuthor changeId name

      -- The change creation
      author <- randomAuthor contributors
      let create = mkEvent' 0 EChangeCreatedEvent author author
          change = mkChange' 0 author

      -- The review
      reviewer <- randomAuthor maintainers
      let review = mkEvent' (duration `div` 2) EChangeReviewedEvent reviewer author

      -- The event lists
      pure [SChange change, SCreation create, SReview review]
