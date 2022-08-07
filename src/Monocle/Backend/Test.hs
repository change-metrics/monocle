--
module Monocle.Backend.Test where

import Control.Exception (bracket_)
import Control.Monad.Random.Lazy
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Google.Protobuf.Timestamp qualified as T
import Monocle.Api.Test qualified
import Monocle.Backend.Documents
import Monocle.Backend.Index qualified as I
import Monocle.Backend.Janitor qualified as J
import Monocle.Backend.Queries qualified as Q
import Monocle.Client.Api qualified
import Monocle.Config qualified as Config
import Monocle.Env
import Monocle.Logging
import Monocle.Prelude
import Monocle.Protob.Crawler qualified as CrawlerPB
import Monocle.Protob.Metric qualified as MetricPB
import Monocle.Protob.Search qualified as MetricPB
import Monocle.Protob.Search qualified as SearchPB
import Monocle.Search.Query (defaultQueryFlavor)
import Monocle.Search.Query qualified as Q
import Relude.Unsafe ((!!))
import Streaming.Prelude qualified as Streaming

fakeDate, fakeDateAlt :: UTCTime
fakeDate = [utctime|2021-05-31 10:00:00|]
fakeDateAlt = [utctime|2021-06-01 20:00:00|]

alice, bob, eve, fakeAuthor, fakeAuthorAlt :: Author
alice = Author "alice" "a"
bob = Author "bob" "b"
eve = Author "eve" "e"
fakeAuthor = Author "John" "John"
fakeAuthorAlt = Author "John Doe/12" "review.opendev.org/John Doe/12"

fakeChange :: EChange
fakeChange =
  EChange
    { echangeId = ""
    , echangeType = EChangeDoc
    , echangeNumber = 1
    , echangeChangeId = "change-id"
    , echangeTitle = ""
    , echangeUrl = ""
    , echangeCommitCount = 1
    , echangeAdditions = 1
    , echangeDeletions = 1
    , echangeChangedFilesCount = 1
    , echangeChangedFiles = [File 0 0 "/fake/path"]
    , echangeText = ""
    , echangeCommits = []
    , echangeRepositoryPrefix = ""
    , echangeRepositoryFullname = ""
    , echangeRepositoryShortname = ""
    , echangeAuthor = fakeAuthor
    , echangeBranch = ""
    , echangeCreatedAt = fakeDate
    , echangeUpdatedAt = fakeDate
    , echangeMergedBy = Nothing
    , echangeTargetBranch = "main"
    , echangeMergedAt = Nothing
    , echangeClosedAt = Nothing
    , echangeDuration = Nothing
    , echangeApproval = Just ["OK"]
    , echangeSelfMerged = Nothing
    , echangeTasksData = Nothing
    , echangeState = EChangeOpen
    , echangeMergeable = ""
    , echangeLabels = []
    , echangeAssignees = []
    , echangeDraft = False
    }

withTenant :: QueryM () -> IO ()
withTenant = withTenantConfig index
 where
  -- todo: generate random name
  index = Config.mkTenant "test-tenant"

withTenantConfig :: Config.Index -> QueryM () -> IO ()
withTenantConfig index cb = bracket_ create delete run
 where
  create = testQueryM index I.ensureIndex
  delete = testQueryM index I.removeIndex
  run = testQueryM index cb

checkEChangeField :: (Show a, Eq a) => BH.DocId -> (EChange -> a) -> a -> QueryM ()
checkEChangeField = _checkField

checkEChangeEventField :: (Show a, Eq a) => BH.DocId -> (EChangeEvent -> a) -> a -> QueryM ()
checkEChangeEventField = _checkField

_checkField :: (FromJSON t, Show a, Eq a) => BH.DocId -> (t -> a) -> a -> QueryM ()
_checkField docId field value = do
  docM <- I.getDocumentById docId
  case docM of
    Just doc -> assertEqual' "Check field match" (field doc) value
    Nothing -> error "Document not found"

checkChangesCount :: Int -> QueryM ()
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
  doTest :: QueryM ()
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
    -- Check we can get the most recently updated change
    dateM <- I.getLastUpdatedDate $ from . echangeRepositoryFullname $ fakeChange1
    assertEqual' "Got most recent change" (Just fakeDateAlt) dateM
    -- Check total count of Change document in the database
    checkChangesCount 2
    -- Check scanSearch has the same count
    withQuery (mkQuery []) $ do
      count <- Streaming.length_ Q.scanSearchId
      assertEqual' "stream count match" 2 count
   where
    checkDocExists' dId = do
      exists <- I.checkDocExists dId
      assertEqual' "check doc exists" exists True
    fakeChange1 = mkFakeChange 1 "My change 1"
    fakeChange1Updated =
      fakeChange1
        { echangeTitle = "My change 1 updated"
        , echangeUpdatedAt = fakeDateAlt
        }
    fakeChange2 = mkFakeChange 2 "My change 2"
    mkFakeChange :: Int -> LText -> EChange
    mkFakeChange number title =
      fakeChange
        { echangeId = "aFakeId-" <> show number
        , echangeNumber = number
        , echangeTitle = title
        , echangeRepositoryFullname = "fakerepo"
        , echangeUrl = "https://fakehost/change/" <> show number
        }

testProjectCrawlerMetadata :: Assertion
testProjectCrawlerMetadata = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    -- Init default crawler metadata and ensure we get the default updated date
    I.initCrawlerMetadata workerGitlab
    lastUpdated <- I.getLastUpdated workerGitlab entityType 0
    assertEqual' "check got oldest updated entity" (Just fakeDefaultDate) $ snd <$> lastUpdated

    -- Update some crawler metadata and ensure we get the oldest (name, last_commit_at)
    I.setLastUpdated crawlerGitlab fakeDateB entity
    I.setLastUpdated crawlerGitlab fakeDateA entityAlt
    lastUpdated' <- I.getLastUpdated workerGitlab entityType 0
    assertEqual' "check got oldest updated entity" (Just ("centos/nova", fakeDateB)) lastUpdated'

    -- Update one crawler and ensure we get the right oldest
    I.setLastUpdated crawlerGitlab fakeDateC entity
    lastUpdated'' <- I.getLastUpdated workerGitlab entityType 0
    assertEqual' "check got oldest updated entity" (Just ("centos/neutron", fakeDateA)) lastUpdated''

    -- Re run init and ensure it was noop
    I.initCrawlerMetadata workerGitlab
    lastUpdated''' <- I.getLastUpdated workerGitlab entityType 0
    assertEqual' "check got oldest updated entity" (Just ("centos/neutron", fakeDateA)) lastUpdated'''

    -- Index a change then verify the just create crawlerMetadata is initialized with
    -- the most recent change updateAt date
    I.indexChanges
      [ fakeChange
          { echangeId = "aFakeId-42"
          , echangeRepositoryFullname = "opendev/nova"
          , echangeUpdatedAt = fakeDateD
          }
      , fakeChange
          { echangeId = "aFakeId-43"
          , echangeRepositoryFullname = "opendev/nova"
          , echangeUpdatedAt = fakeDateC
          }
      ]
    I.initCrawlerMetadata workerGerrit
    lastUpdated'''' <- I.getLastUpdated workerGerrit entityType 0
    assertEqual' "check got oldest updated entity" (Just ("opendev/nova", fakeDateD)) lastUpdated''''
   where
    entityType = CrawlerPB.EntityEntityProjectName ""
    entity = Project "centos/nova"
    entityAlt = Project "centos/neutron"
    crawlerGitlab = "test-crawler-gitlab"
    crawlerGerrit = "test-crawler-gerrit"
    workerGitlab =
      let name = crawlerGitlab
          update_since = show fakeDefaultDate
          provider =
            let gitlab_url = Just "https://localhost"
                gitlab_token = Nothing
                gitlab_repositories = Just ["nova", "neutron"]
                gitlab_organization = "centos"
             in Config.GitlabProvider Config.Gitlab {..}
       in Config.Crawler {..}
    workerGerrit =
      let name = crawlerGerrit
          update_since = show fakeDefaultDate
          provider =
            let gerrit_url = "https://localhost"
                gerrit_login = Nothing
                gerrit_password = Nothing
                gerrit_prefix = Nothing
                gerrit_repositories = Just ["opendev/nova"]
             in Config.GerritProvider Config.Gerrit {..}
       in Config.Crawler {..}
    fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
    fakeDateB = [utctime|2021-05-31 10:00:00|]
    fakeDateA = [utctime|2021-06-01 20:00:00|]
    fakeDateC = [utctime|2021-06-02 23:00:00|]
    fakeDateD = [utctime|2021-10-01 01:00:00|]

testOrganizationCrawlerMetadata :: Assertion
testOrganizationCrawlerMetadata = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    -- Init crawler entities metadata and check we get the default date
    I.initCrawlerMetadata worker
    lastUpdated <- I.getLastUpdated worker entityType 0
    assertEqual' "check got oldest updated entity" (Just fakeDefaultDate) $ snd <$> lastUpdated

    -- TODO(fbo) extract Server.AddProjects and use it directly
    I.initCrawlerEntities (Project <$> ["nova", "neutron"]) worker
    projectA <- I.checkDocExists $ getProjectCrawlerDocId "nova"
    projectB <- I.checkDocExists $ getProjectCrawlerDocId "neutron"
    assertEqual' "Check crawler metadata for projectA present" True projectA
    assertEqual' "Check crawler metadata for projectB present" True projectB

    -- Update the crawler metadata
    I.setLastUpdated crawlerName fakeDateA $ Organization "gitlab-org"
    lastUpdated' <- I.getLastUpdated worker entityType 0
    assertEqual' "check got oldest updated entity" (Just ("gitlab-org", fakeDateA)) lastUpdated'
   where
    entityType = CrawlerPB.EntityEntityOrganizationName ""
    fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
    fakeDateA = [utctime|2021-06-01 20:00:00|]
    crawlerName = "test-crawler"
    getProjectCrawlerDocId =
      I.getCrawlerMetadataDocId
        crawlerName
        ( I.getCrawlerTypeAsText (CrawlerPB.EntityEntityProjectName "")
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

testTaskDataCrawlerMetadata :: Assertion
testTaskDataCrawlerMetadata = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    -- Init default crawler metadata and ensure we get the default updated date
    I.initCrawlerMetadata workerGithub
    lastUpdated <- I.getLastUpdated workerGithub entityType 0
    assertEqual' "check got oldest updated entity" (Just fakeDefaultDate) $ snd <$> lastUpdated

    -- Update some crawler metadata and ensure we get the oldest (name, last_commit_at)
    I.setLastUpdated crawlerGithub fakeDateB entity
    lastUpdated' <- I.getLastUpdated workerGithub entityType 0
    assertEqual' "check got oldest updated entity" (Just ("opendev/nova", fakeDateB)) lastUpdated'

    -- Run again the init
    I.initCrawlerMetadata workerGithub
    lastUpdated'' <- I.getLastUpdated workerGithub entityType 0
    assertEqual' "check got oldest updated entity" (Just ("opendev/nova", fakeDateB)) lastUpdated''
   where
    entityType = CrawlerPB.EntityEntityTdName ""
    entity = TaskDataEntity "opendev/nova"
    crawlerGithub = "test-crawler-github"
    workerGithub =
      let name = crawlerGithub
          update_since = show fakeDefaultDate
          provider =
            let github_url = Nothing
                github_token = Nothing
                github_repositories = Just ["nova"]
                github_organization = "opendev"
             in Config.GithubProvider Config.Github {..}
       in Config.Crawler {..}
    fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
    fakeDateB = [utctime|2021-05-31 10:00:00|]

testEnsureConfig :: Assertion
testEnsureConfig = bracket_ create delete doTest
 where
  wrap :: QueryM () -> IO ()
  wrap action = do
    bhEnv <- mkEnv'
    let qt = QueryConfig $ Config.Config Nothing Nothing [tenantConfig]
    runQueryTarget bhEnv qt action

  create = do
    testQueryM tenantConfig I.ensureIndexSetup
    wrap I.ensureConfigIndex
  delete = do
    testQueryM tenantConfig I.removeIndex
    wrap I.removeIndex
  doTest = wrap $ do
    (currentVersion, _) <- I.getConfigVersion
    assertEqual' "Check expected Config Index" I.configVersion currentVersion
  tenantConfig = Config.mkTenant "test-index"

testUpgradeConfigV3 :: Assertion
testUpgradeConfigV3 = do
  -- Index some events, run upgradeConfigV3, and check self_merged added on EChangeMergedEvent
  withTenant $ do
    let evt1 = emptyEvent {echangeeventType = EChangeCommentedEvent, echangeeventId = "1"}
        -- emptyEvent set the same author for author and onAuthor attribute
        evt2 = emptyEvent {echangeeventType = EChangeMergedEvent, echangeeventId = "2"}
        evt3 =
          emptyEvent
            { echangeeventType = EChangeMergedEvent
            , echangeeventId = "3"
            , echangeeventOnAuthor = fakeAuthorAlt
            }
    I.indexEvents [evt1, evt2, evt3]
    count <- I.upgradeConfigV3
    assertEqual' "Expect document count updated" count 1
    (Just evt1') <- I.getChangeEventById $ I.getEventDocId evt1
    assertEqual' "Expect not self_merged attr in event" (echangeeventSelfMerged evt1') Nothing
    (Just evt2') <- I.getChangeEventById $ I.getEventDocId evt2
    assertEqual' "Expect self_merged attr in event" (echangeeventSelfMerged evt2') (Just True)
    (Just evt3') <- I.getChangeEventById $ I.getEventDocId evt3
    assertEqual' "Expect not self_merged attr in event" (echangeeventSelfMerged evt3') Nothing

testUpgradeConfigV4 :: Assertion
testUpgradeConfigV4 = do
  -- Index a change with negative duration, run upgradeConfigV4, and check for absolute value
  withTenant $ do
    let change1 =
          emptyChange
            { echangeId = "change1"
            , echangeState = from EChangeMerged
            , echangeDuration = Just (-3600)
            }
        change2 =
          emptyChange
            { echangeId = "change2"
            , echangeState = from EChangeMerged
            , echangeDuration = Just 7200
            }
    I.indexChanges [change1, change2]
    count <- I.upgradeConfigV4
    assertEqual' "Expect document count updated" 1 count
    (Just change1') <- I.getChangeById $ I.getChangeDocId change1
    assertEqual' "Expect abs duration value" (echangeDuration change1') (abs <$> echangeDuration change1)

testUpgradeConfigV1 :: Assertion
testUpgradeConfigV1 = do
  -- Index docs, run upgradeConfigV1, and check project crawler MD state
  withTenantConfig tenantConfig $ do
    -- Index some events and set lastCommitAt for the first (repoGH1 and repoGL1) project crawler MD
    setDocs crawlerGH crawlerGHName "org/repoGH1" "org/repoGH2"
    setDocs crawlerGL crawlerGLName "org/repoGL1" "org/repoGL2"
    -- Run upgrade V1 function
    I.upgradeConfigV1
    -- Fetch crawler MD for project repoGH1 and expect previously set date
    crawlerMDrepoGH1 <- getCrawlerProjectMD crawlerGHName "org/repoGH1"
    assertCommitAtDate crawlerMDrepoGH1 createdAtDate
    -- Fetch crawler MD for project repoGH2 and expect date set by the upgrade function
    crawlerMDrepoGH2 <- getCrawlerProjectMD crawlerGHName "org/repoGH2"
    assertCommitAtDate crawlerMDrepoGH2 createdAtDate
    -- Fetch crawler MD for project repoGL1 and expect previously set date
    crawlerMDrepoGL1 <- getCrawlerProjectMD crawlerGLName "org/repoGL1"
    assertCommitAtDate crawlerMDrepoGL1 createdAtDate
    -- Fetch crawler MD for project repoGL2 and expect default crawler date
    crawlerMDrepoGL2 <- getCrawlerProjectMD crawlerGLName "org/repoGL2"
    assertCommitAtDate crawlerMDrepoGL2 defaultCrawlerDate
 where
  assertCommitAtDate :: Maybe ECrawlerMetadata -> UTCTime -> QueryM ()
  assertCommitAtDate (Just (ECrawlerMetadata ECrawlerMetadataObject {ecmLastCommitAt, ecmCrawlerTypeValue})) d =
    assertEqual' ("Check crawler Metadata lastCommitAt for repo: " <> from ecmCrawlerTypeValue) d ecmLastCommitAt
  assertCommitAtDate Nothing _ = error "Unexpected missing lastCommitAt date"
  getCrawlerProjectMD :: Text -> Text -> QueryM (Maybe ECrawlerMetadata)
  getCrawlerProjectMD crawlerName repoName = I.getDocumentById getCrawlerProjectMDDocID
   where
    getCrawlerProjectMDDocID =
      let entity = Project repoName
       in I.getCrawlerMetadataDocId crawlerName (from entity) (getEntityName entity)
  setDocs :: Config.Crawler -> Text -> Text -> Text -> QueryM ()
  setDocs crawler crawlerName repo1 repo2 = do
    -- Init crawler metadata
    I.initCrawlerMetadata crawler
    -- Index two events
    I.indexChanges
      [ emptyChange
          { echangeId = "fakeId-" <> from repo1
          , echangeRepositoryFullname = from repo1
          , echangeUpdatedAt = createdAtDate
          }
      , emptyChange
          { echangeId = "fakeId-" <> from repo2
          , echangeRepositoryFullname = from repo2
          , echangeUpdatedAt = createdAtDate
          }
      ]
    -- Set crawler metadata (lastCommitAt) (for project entity) to a correct date
    I.setLastUpdated crawlerName createdAtDate (Project repo1)
  createdAtDate = [utctime|2020-01-02 00:00:00|]
  defaultCrawlerDate = [utctime|2020-01-01 00:00:00|]
  crawlerGHName = "crawlerGH"
  crawlerGH =
    let providerGH = Config.GithubProvider $ Config.Github "org" (Just ["repoGH1", "repoGH2"]) Nothing Nothing
     in Config.Crawler crawlerGHName providerGH "2020-01-01"
  crawlerGLName = "crawlerGL"
  crawlerGL =
    let providerGL = Config.GitlabProvider $ Config.Gitlab "org" (Just ["repoGL1", "repoGL2"]) Nothing Nothing
     in Config.Crawler crawlerGLName providerGL "2020-01-01"
  tenantConfig :: Config.Index
  tenantConfig =
    Config.Index
      { name = "test-tenant"
      , crawlers = [crawlerGH, crawlerGL]
      , crawlers_api_key = Nothing
      , projects = Nothing
      , idents = Nothing
      , search_aliases = Nothing
      }

testJanitorWipeCrawler :: Assertion
testJanitorWipeCrawler = withTenant $ local updateEnv doTest
 where
  crawlerGerrit = "test-crawler-gerrit"
  fakeDefaultDate = [utctime|2020-01-01 00:00:00|]
  workerGerrit =
    let name = crawlerGerrit
        update_since = show fakeDefaultDate
        provider =
          let gerrit_url = "https://localhost"
              gerrit_login = Nothing
              gerrit_password = Nothing
              gerrit_prefix = Nothing
              gerrit_repositories =
                Just
                  [ "opendev/nova"
                  , "^opendev/neutron.*"
                  ]
           in Config.GerritProvider Config.Gerrit {..}
     in Config.Crawler {..}
  updateEnv :: QueryEnv -> QueryEnv
  updateEnv orig = orig {tenant = QueryWorkspace tenant}
   where
    tenant =
      Config.Index
        { name = "test-tenant"
        , crawlers = [workerGerrit]
        , crawlers_api_key = Nothing
        , projects = Nothing
        , idents = Nothing
        , search_aliases = Nothing
        }
  doTest :: QueryM ()
  doTest = do
    I.initCrawlerMetadata workerGerrit
    I.indexChanges
      [ emptyChange
          { echangeId = "aFakeId-42"
          , echangeRepositoryFullname = "opendev/nova"
          }
      , emptyChange
          { echangeId = "aFakeId-43"
          , echangeRepositoryFullname = "opendev/neutron"
          }
      ]
    I.indexEvents
      [ emptyEvent
          { echangeeventId = "aFakeEventId-43"
          , echangeeventRepositoryFullname = "opendev/nova"
          }
      ]
    count <- withQuery sQuery Q.countDocs
    assertEqual' "Ensure expected amount of docs" 5 count
    void $ J.wipeCrawlerData crawlerGerrit
    count' <- withQuery sQuery Q.countDocs
    assertEqual' "Ensure expected amount of docs after wipe" 1 count'
   where
    sQuery = mkQuery [BH.MatchAllQuery Nothing]

testJanitorUpdateIdents :: Assertion
testJanitorUpdateIdents = do
  withTenantConfig tenantConfig doUpdateIndentOnChangesTest
  withTenantConfig tenantConfig doUpdateIndentOnEventsTest
 where
  tenantConfig :: Config.Index
  tenantConfig =
    Config.Index
      { name = "test-tenant"
      , crawlers = []
      , crawlers_api_key = Nothing
      , projects = Nothing
      , idents = Just [mkIdent ["github.com/john"] "John Doe"]
      , search_aliases = Nothing
      }
  mkIdent :: [Text] -> Text -> Config.Ident
  mkIdent uid = Config.Ident uid Nothing
  expectedAuthor = Author "John Doe" "github.com/john"

  doUpdateIndentOnEventsTest :: QueryM ()
  doUpdateIndentOnEventsTest = do
    I.indexEvents [evt1, evt2]
    count <- J.updateIdentsOnEvents
    assertEqual' "Ensure updated events count" 1 count
    -- Ensure evt1 got the ident update
    checkEChangeEventField (I.getEventDocId evt1) echangeeventAuthor (Just expectedAuthor)
    checkEChangeEventField (I.getEventDocId evt1) echangeeventOnAuthor expectedAuthor
    -- Ensure evt2 is the same
    evt2' <- I.getChangeEventById $ I.getEventDocId evt2
    assertEqual' "Ensure event not changed" evt2' $ Just evt2
   where
    evt1 = mkEventWithAuthor "e1" (Author "john" "github.com/john")
    evt2 = mkEventWithAuthor "e2" (Author "paul" "github.com/paul")
    mkEventWithAuthor ::
      -- eventId
      Text ->
      -- the Author used to build Author fields (author, mergeBy, assignees, ...)
      Author ->
      EChangeEvent
    mkEventWithAuthor eid eAuthor =
      mkEvent 0 fakeDate EChangeCommentedEvent eAuthor eAuthor (from eid) mempty

  doUpdateIndentOnChangesTest :: QueryM ()
  doUpdateIndentOnChangesTest = do
    I.indexChanges [change1, change2, change3]
    count <- J.updateIdentsOnChanges
    -- change1 and change3 will be updated
    assertEqual' "Ensure updated changes count" 2 count
    -- Ensure change1 got the ident update
    checkEChangeField (I.getChangeDocId change1) echangeAuthor expectedAuthor
    checkEChangeField (I.getChangeDocId change1) echangeMergedBy (Just expectedAuthor)
    checkEChangeField (I.getChangeDocId change1) echangeAssignees [expectedAuthor]
    checkEChangeField (I.getChangeDocId change1) echangeCommits [mkCommit expectedAuthor]
    -- Ensure change2 is the same
    change2' <- I.getChangeById $ I.getChangeDocId change2
    assertEqual' "Ensure change not changed" change2' $ Just change2
    -- Ensure change3 got its Author fields updated - Only check author to be brief
    -- Ident got reverted because no Ident match github.com/jane in config
    -- then default is to remove the "<provider-host>/" prefix
    checkEChangeField (I.getChangeDocId change3) echangeAuthor expectedAuthor'
   where
    expectedAuthor' = Author "jane" "github.com/jane"
    change1 = mkChangeWithAuthor "c1" (Author "john" "github.com/john")
    change2 = mkChangeWithAuthor "c2" (Author "paul" "github.com/paul")
    change3 = mkChangeWithAuthor "c3" (Author "Ident will revert" "github.com/jane")
    mkChangeWithAuthor ::
      -- changeId
      Text ->
      -- the Author used to build Author fields (author, mergeBy, assignees, ...)
      Author ->
      EChange
    mkChangeWithAuthor did cAuthor =
      (mkChange 0 fakeDate cAuthor (from did) mempty EChangeOpen)
        { echangeMergedBy = Just cAuthor
        , echangeAssignees = [cAuthor]
        , echangeCommits = [mkCommit cAuthor]
        }
    mkCommit :: Author -> Commit
    mkCommit cAuthor =
      Commit
        { commitSha = mempty
        , commitAuthor = cAuthor
        , commitCommitter = cAuthor
        , commitAuthoredAt = fakeDate
        , commitCommittedAt = fakeDate
        , commitAdditions = 0
        , commitDeletions = 0
        , commitTitle = mempty
        }

reviewers :: [Author]
reviewers = [alice, bob]

scenarioProject :: LText -> ScenarioProject
scenarioProject name =
  SProject name reviewers [alice] [eve]

testAchievements :: Assertion
testAchievements = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    indexScenario (nominalMerge (scenarioProject "openstack/nova") "42" fakeDate 3600)

    -- Try query
    now <- getCurrentTime
    agg <- head . fromMaybe (error "noagg") . nonEmpty <$> Q.getProjectAgg (query now)
    assertEqual' "event found" (Q.epbType agg) "Change"
    assertEqual' "event count match" (Q.epbCount agg) 1
   where
    conf = mkConfig "test"
    query now = case (Q.queryGet $ Q.load now mempty conf "state:merged") id (Just defaultQueryFlavor) of
      [x] -> x
      _ -> error "Could not compile query"

defaultQuery :: Q.Query
defaultQuery =
  let queryGet _ = const []
      queryBounds =
        ( [utctime|2000-01-01 00:00:00|]
        , [utctime|2099-12-31 23:59:59|]
        )
      queryMinBoundsSet = False
   in Q.Query {..}

testGetMetrics :: Assertion
testGetMetrics = withTenantConfig tenant $ do
  -- Add data to the index
  indexScenario (nominalMerge (scenarioProject "openstack/nova") "42" fakeDate 1800)

  -- Start the API
  liftIO . Monocle.Api.Test.withTestApi env $ \_logger client -> do
    -- Get basic metric
    resp <- Monocle.Client.Api.metricGet client (mkReq "time_to_merge")
    assertEqual
      "Metric response match"
      (mkResp . MetricPB.GetResponseResultFloatValue $ 1800)
      resp

    -- Get bad metric
    badResp <- Monocle.Client.Api.metricGet client (mkReq "unknown")
    assertEqual
      "Invalid response match"
      (mkResp . MetricPB.GetResponseResultError $ "Unknown metric: unknown")
      badResp
 where
  mkResp = MetricPB.GetResponse . Just
  mkReq getRequestMetric =
    MetricPB.GetRequest
      { MetricPB.getRequestIndex = from tenantName
      , MetricPB.getRequestUsername = ""
      , MetricPB.getRequestQuery = "from:2021-01-01 to:2022-01-01"
      , MetricPB.getRequestMetric = getRequestMetric
      , MetricPB.getRequestOptions = Nothing
      }
  env = Monocle.Api.Test.mkAppEnv tenant
  tenantName = "test-metric-tenant"
  tenant = Config.mkTenant tenantName

testReposSummary :: Assertion
testReposSummary = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    indexScenario (nominalMerge (scenarioProject "openstack/nova") "42" fakeDate 3600)
    indexScenario (nominalMerge (scenarioProject "openstack/neutron") "43" fakeDate 3600)
    indexScenario (nominalMerge (scenarioProject "openstack/neutron") "44" fakeDate 3600)
    indexScenario (nominalOpen (scenarioProject "openstack/swift") "45" fakeDate 3600)

    results <- withQuery defaultQuery Q.getReposSummary
    assertEqual'
      "Check buckets names"
      [ Q.RepoSummary
          { fullname = "openstack/neutron"
          , createdChanges = 2
          , abandonedChanges = 0
          , mergedChanges = 2
          , updatedChanges = 0
          , openChanges = 0
          }
      , Q.RepoSummary
          { fullname = "openstack/nova"
          , createdChanges = 1
          , abandonedChanges = 0
          , mergedChanges = 1
          , updatedChanges = 0
          , openChanges = 0
          }
      , Q.RepoSummary
          { fullname = "openstack/swift"
          , createdChanges = 1
          , abandonedChanges = 0
          , mergedChanges = 0
          , updatedChanges = 1
          , openChanges = 1
          }
      ]
      results

testTopAuthors :: Assertion
testTopAuthors = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    -- Prapare data
    let nova = SProject "openstack/nova" [alice] [alice] [eve]
    let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
    traverse_ (indexScenarioNM nova) ["42", "43"]
    traverse_ (indexScenarioNO neutron) ["142", "143"]

    -- Check for expected metrics
    withQuery defaultQuery $ do
      results <- fromJust <$> Q.runMetricTop Q.metricChangeAuthors 10
      assertEqual'
        "Check metricChangeAuthors Top"
        (V.fromList [Q.TermCount {tcTerm = "eve", tcCount = 4}])
        (Q.tscData results)
      results' <- Q.getMostActiveAuthorByChangeMerged 10
      assertEqual'
        "Check getMostActiveAuthorByChangeMerged count"
        [Q.TermResult {trTerm = "eve", trCount = 2}]
        (Q.tsrTR results')
      results'' <- fromJust <$> Q.runMetricTop Q.metricReviewAuthors 10
      assertEqual'
        "Check metricReviewAuthors Top"
        ( V.fromList
            [ Q.TermCount {tcTerm = "alice", tcCount = 2}
            , Q.TermCount {tcTerm = "bob", tcCount = 2}
            ]
        )
        (Q.tscData results'')
      results''' <- fromJust <$> Q.runMetricTop Q.metricCommentAuthors 10
      assertEqual'
        "Check metricCommentAuthors Top"
        (V.fromList [Q.TermCount {tcTerm = "alice", tcCount = 2}])
        (Q.tscData results''')
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
  doTest :: QueryM ()
  doTest = do
    -- Prapare data
    let nova = SProject "openstack/nova" [bob] [alice] [eve]
    let neutron = SProject "openstack/neutron" [alice] [eve] [bob]
    let horizon = SProject "openstack/horizon" [alice] [alice] [alice]
    traverse_ (indexScenarioNM nova) ["42", "43"]
    traverse_ (indexScenarioNM neutron) ["142"]
    traverse_ (indexScenarioNM horizon) ["242"]

    -- Check for expected metrics
    withQuery defaultQuery $ do
      results <- Q.getAuthorsPeersStrength 10
      assertEqual'
        "Check getAuthorsPeersStrength results"
        [ Q.PeerStrengthResult
            { psrAuthor = "eve"
            , psrPeer = "bob"
            , psrStrength = 4
            }
        , Q.PeerStrengthResult
            { psrAuthor = "bob"
            , psrPeer = "alice"
            , psrStrength = 2
            }
        ]
        results

testGetNewContributors :: Assertion
testGetNewContributors = withTenant doTest
 where
  indexScenario' project fakeDate' cid = indexScenario (nominalMerge project cid fakeDate' 3600)
  doTest :: QueryM ()
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
                ( addUTCTime 3600 fakeDate
                , [utctime|2099-12-31 23:59:59|]
                )
              queryMinBoundsSet = True
           in Q.Query {..}

    withQuery query $ do
      results <- Q.getNewContributors
      assertEqual'
        "Check getNewContributors results"
        [Q.TermResult {trTerm = "bob", trCount = 1}]
        results

testLifecycleStats :: Assertion
testLifecycleStats = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    traverse_ (indexScenarioNM (SProject "openstack/nova" [alice] [bob] [eve])) ["42", "43"]
    let query =
          let queryGet _ = const []
              queryBounds =
                ( addUTCTime (-3600) fakeDate
                , addUTCTime 3600 fakeDate
                )
              queryMinBoundsSet = True
           in Q.Query {..}

    withQuery query $ do
      res <- Q.getLifecycleStats
      liftIO $ assertBool "stats exist" (not $ null $ SearchPB.lifecycleStatsCreatedHisto res)

testGetActivityStats :: Assertion
testGetActivityStats = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    -- Prapare data
    let nova = SProject "openstack/nova" [alice] [alice] [eve]
    let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
    traverse_ (indexScenarioNM nova) ["42", "43"]
    traverse_ (indexScenarioNO neutron) ["142", "143"]

    let query =
          let queryGet _ = const []
              queryBounds =
                ( addUTCTime (-3600) fakeDate
                , addUTCTime 3600 fakeDate
                )
              queryMinBoundsSet = True
           in Q.Query {..}

    withQuery query $ do
      results <- Q.getActivityStats
      assertEqual'
        "Check getActivityStats result"
        ( SearchPB.ActivityStats
            1
            1
            2
            ( V.fromList
                [ MetricPB.HistoInt {histoIntDate = "2021-05-31 09:00", histoIntCount = 0}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 10:00", histoIntCount = 1}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 11:00", histoIntCount = 0}
                ]
            )
            ( V.fromList
                [ MetricPB.HistoInt {histoIntDate = "2021-05-31 09:00", histoIntCount = 0}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 10:00", histoIntCount = 2}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 11:00", histoIntCount = 0}
                ]
            )
            ( V.fromList
                [ MetricPB.HistoInt {histoIntDate = "2021-05-31 09:00", histoIntCount = 0}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 10:00", histoIntCount = 1}
                , MetricPB.HistoInt {histoIntDate = "2021-05-31 11:00", histoIntCount = 0}
                ]
            )
        )
        results

testGetChangesTops :: Assertion
testGetChangesTops = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    let nova = SProject "openstack/nova" [alice] [alice] [eve]
    let neutron = SProject "openstack/neutron" [bob] [alice] [eve]
    traverse_ (indexScenarioNM nova) ["42", "43"]
    traverse_ (indexScenarioNO neutron) ["142", "143"]

    withQuery defaultQuery $ do
      results <- Q.getChangesTops 10
      assertEqual'
        "Check getChangesTops result"
        ( SearchPB.ChangesTops
            { changesTopsAuthors =
                Just
                  ( MetricPB.TermsCountInt
                      { termsCountIntTermcount =
                          V.fromList
                            [ MetricPB.TermCountInt
                                { termCountIntTerm = "eve"
                                , termCountIntCount = 4
                                }
                            ]
                      , termsCountIntTotalHits = 4
                      }
                  )
            , changesTopsRepos =
                Just
                  ( MetricPB.TermsCountInt
                      { termsCountIntTermcount =
                          V.fromList
                            [ MetricPB.TermCountInt
                                { termCountIntTerm = "openstack/neutron"
                                , termCountIntCount = 2
                                }
                            , MetricPB.TermCountInt
                                { termCountIntTerm = "openstack/nova"
                                , termCountIntCount = 2
                                }
                            ]
                      , termsCountIntTotalHits = 4
                      }
                  )
            , changesTopsApprovals =
                Just
                  ( MetricPB.TermsCountInt
                      { termsCountIntTermcount =
                          V.fromList
                            [ MetricPB.TermCountInt
                                { termCountIntTerm = "OK"
                                , termCountIntCount = 4
                                }
                            ]
                      , termsCountIntTotalHits = 4
                      }
                  )
            }
        )
        results

testGetSuggestions :: Assertion
testGetSuggestions = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    tEnv <- ask
    let nova = SProject "openstack/nova" [alice] [alice] [eve]
    let neutron = SProject "openstack/neutron" [eve] [alice] [bob]
    traverse_ (indexScenarioNM nova) ["42", "43"]
    traverse_ (indexScenarioNO neutron) ["142", "143"]
    let ws = case tenant tEnv of
          QueryWorkspace x -> x
          QueryConfig _ -> error "Can't get config suggestions"

    withQuery defaultQuery $ do
      results <- Q.getSuggestions ws
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
            mempty
        )
        results

testGetAllAuthorsMuid :: Assertion
testGetAllAuthorsMuid = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    traverse_ (indexScenarioNM $ SProject "openstack/nova" [alice] [alice] [eve]) ["42", "43"]
    withQuery defaultQuery $ do
      results <- Q.getAllAuthorsMuid'
      assertEqual' "Check getAllAuthorsMuid result" ["alice", "eve"] results

testAuthorCache :: Assertion
testAuthorCache = withTenant doTest
 where
  doTest = do
    -- Index a change and some events
    traverse_ (indexScenarioNM $ SProject "openstack/nova" [alice] [alice] [eve]) ["42", "43"]

    -- Validate that populate clear and index 2 authors in the cache
    added <- I.populateAuthorCache
    assertEqual' "Check author cache populated with" 2 added
    added' <- I.populateAuthorCache
    assertEqual' "Check author cache populated with" 2 added'

    -- Validate that addCachedAuthors extracts the new author from the event and adds in the cache
    I.addCachedAuthors
      [ mkEvent
          0
          fakeDate
          EChangeCreatedEvent
          fakeAuthorAlt
          fakeAuthorAlt
          "change-44"
          "openstack/nova"
      ]
    resp <- I.getAuthorCache
    assertEqual' "Check author cache populated with" 3 $ length resp
    assertEqual'
      "Check author cache populated with"
      [ CachedAuthor
          { caType = ECachedAuthor
          , caCachedAuthorMuid = "alice"
          }
      , CachedAuthor
          { caType = ECachedAuthor
          , caCachedAuthorMuid = "eve"
          }
      , CachedAuthor
          { caType = ECachedAuthor
          , caCachedAuthorMuid = "John Doe/12"
          }
      ]
      resp

    -- Validate that we can search in the author cache
    authors <- I.searchAuthorCache "doe"
    assertEqual' "Check search author" ["John Doe/12"] authors

mkTaskData :: LText -> SearchPB.TaskData
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
   in SearchPB.TaskData {..}

testTaskDataAdd :: Assertion
testTaskDataAdd = withTenant doTest
 where
  doTest :: QueryM ()
  doTest = do
    let nova = SProject "openstack/nova" [alice] [alice] [eve]
    traverse_ (indexScenarioNM nova) ["42", "43", "44"]

    -- Send Task data with a matching changes
    let td42 = mkTaskData "42"
        td43 = mkTaskData "43"
        crawlerName = "crawler"
    void $ I.taskDataAdd crawlerName [td42, td43]
    -- Ensure only changes 42 and 43 got a Task data associated
    changes <- I.runScanSearch $ I.getChangesByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"])
    assertEqual'
      "Check adding matching taskData"
      [ ("44", Nothing)
      , ("43", Just [I.toETaskData crawlerName td43])
      , ("42", Just [I.toETaskData crawlerName td42])
      ]
      ((\EChange {..} -> (echangeId, echangeTasksData)) <$> changes)
    -- Ensure associated ChangeEvents got the Task data attibutes
    events <- I.runScanSearch $ I.getChangesEventsByURL (map ("https://fakeprovider/" <>) ["42", "43", "44"])
    let (withTD, withoutTD) = partition (isJust . echangeeventTasksData) events
        createdEventWithTD =
          filter
            (\e -> (e & echangeeventType) == EChangeCreatedEvent)
            withTD
    assertEqual' "Check events count that got a Task data" 8 (length withTD)
    assertEqual' "Check events count that miss a Task data" 4 (length withoutTD)
    assertEqual'
      "Check Change events got the task data attribute"
      [ ("ChangeCreatedEvent-42", Just [I.toETaskData crawlerName td42])
      , ("ChangeCreatedEvent-43", Just [I.toETaskData crawlerName td43])
      ]
      ( ( \EChangeEvent {..} ->
            (echangeeventId, echangeeventTasksData)
        )
          <$> createdEventWithTD
      )

    -- Send a Task data w/o a matching change (orphan task data)
    let td = mkTaskData "45"
    void $ I.taskDataAdd crawlerName [td]
    -- Ensure the Task data has been stored as orphan (we can find it by its url as DocId)
    let tdid = (td & SearchPB.taskDataUrl) <> (td & SearchPB.taskDataChangeUrl)
    orphanTdM <- getOrphanTd . from $ tdid
    let expectedTD = I.toETaskData crawlerName td
    assertEqual'
      "Check Task data stored as Orphan Task Data"
      ( Just
          ( EChangeOrphanTD
              { echangeorphantdId = I.getDocID $ from tdid
              , echangeorphantdType = EOrphanTaskData
              , echangeorphantdTasksData = expectedTD
              }
          )
      )
      orphanTdM

    -- Send the same orphan task data with an updated field and ensure it has been
    -- updated in the Database
    let td' = td {SearchPB.taskDataSeverity = "urgent"}
    void $ I.taskDataAdd crawlerName [td']
    orphanTdM' <- getOrphanTd . from $ tdid
    let expectedTD' = expectedTD {tdSeverity = "urgent"}
    assertEqual'
      "Check Task data stored as Orphan Task Data"
      ( Just
          ( EChangeOrphanTD
              { echangeorphantdId = I.getDocID $ from tdid
              , echangeorphantdType = EOrphanTaskData
              , echangeorphantdTasksData = expectedTD'
              }
          )
      )
      orphanTdM'

  getOrphanTd :: Text -> QueryM (Maybe EChangeOrphanTD)
  getOrphanTd url = I.getDocumentById $ I.getBHDocID url

testTaskDataAdoption :: Assertion
testTaskDataAdoption = withTenant doTest
 where
  doTest :: QueryM ()
  doTest =
    do
      -- Send Task data w/o a matching change (orphan task data)
      let td42 = mkTaskData "42"
          td43 = mkTaskData "43"
      void $ I.taskDataAdd "crawlerName" [td42, td43]
      oTDs <- I.getOrphanTaskDataByChangeURL $ from . SearchPB.taskDataChangeUrl <$> [td42, td43]
      assertEqual' "Check we can fetch the orphan task data" 2 (length oTDs)

      -- Index a change and related events
      let scenario = nominalMerge (SProject "openstack/nova" [alice, bob] [alice] [eve]) "42" fakeDate 3600
          events = catMaybes $ getScenarioEvtObj <$> scenario
          changes = catMaybes $ getScenarioChangeObj <$> scenario
      indexScenario scenario
      I.updateChangesAndEventsFromOrphanTaskData changes events
      -- Check that the matching task data has been adopted
      oTDs' <- I.getOrphanTaskDataByChangeURL $ from . SearchPB.taskDataChangeUrl <$> [td42, td43]
      assertEqual' "Check remaining one orphan TD" 1 (length oTDs')
      -- Check that change and related events got the task data attribute
      changes' <- I.runScanSearch $ I.getChangesByURL [changeUrl]
      events' <- I.runScanSearch $ I.getChangesEventsByURL [changeUrl]
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
  i <- getRandomR (0, n - 1)
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
  echangeeventSelfMerged = Nothing
  echangeeventCreatedAt = fakeDate
  echangeeventOnCreatedAt = fakeDate
  echangeeventApproval = Nothing
  echangeeventTasksData = Nothing
  echangeeventLabels = mempty
  echangeeventDuration = Nothing

showEvents :: [ScenarioEvent] -> Text
showEvents xs = Text.intercalate ", " $ sort (map go xs)
 where
  author = maybe "no-author" (from . authorMuid)
  date = from . formatTime defaultTimeLocale "%Y-%m-%d"
  go ev = case ev of
    SChange EChange {..} -> "Change[" <> from echangeChangeId <> "]"
    SCreation EChangeEvent {..} ->
      ("Change[" <> date echangeeventOnCreatedAt <> " ")
        <> (from echangeeventChangeId <> " created by " <> author echangeeventAuthor)
        <> "]"
    SComment EChangeEvent {..} -> "Commented[" <> author echangeeventAuthor <> "]"
    SReview EChangeEvent {..} -> "Reviewed[" <> author echangeeventAuthor <> "]"
    SMerge EChangeEvent {..} -> "Merged[" <> date echangeeventOnCreatedAt <> "]"

-- Tests scenario data types

-- | 'ScenarioProject' is a data type to define a project for a scenario.
data ScenarioProject = SProject
  { name :: LText
  , maintainers :: [Author]
  , commenters :: [Author]
  , contributors :: [Author]
  }

-- | 'ScenarioEvent' is a type of event generated for a given scenario.
data ScenarioEvent
  = SChange EChange
  | SCreation EChangeEvent
  | SReview EChangeEvent
  | SComment EChangeEvent
  | SMerge EChangeEvent

indexScenario :: [ScenarioEvent] -> QueryM ()
indexScenario xs = sequence_ $ indexDoc <$> xs
 where
  indexDoc = \case
    SChange d -> I.indexChanges [d]
    SCreation d -> I.indexEvents [d]
    SReview d -> I.indexEvents [d]
    SComment d -> I.indexEvents [d]
    SMerge d -> I.indexEvents [d]

indexScenarioNM :: ScenarioProject -> LText -> QueryM ()
indexScenarioNM project cid = indexScenario (nominalMerge project cid fakeDate 3600)

indexScenarioNO :: ScenarioProject -> LText -> QueryM ()
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
    { echangeType = EChangeDoc
    , echangeId = changeId
    , echangeState = state'
    , echangeRepositoryFullname = name
    , echangeCreatedAt = mkDate ts start
    , echangeAuthor = author
    , echangeChangeId = "change-" <> changeId
    , echangeUrl = "https://fakeprovider/" <> changeId
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
    { echangeeventAuthor = Just author
    , echangeeventOnAuthor = onAuthor
    , echangeeventType = etype
    , echangeeventRepositoryFullname = name
    , echangeeventId = from etype <> "-" <> changeId
    , echangeeventCreatedAt = mkDate ts start
    , echangeeventOnCreatedAt = mkDate ts start
    , echangeeventChangeId = "change-" <> changeId
    , echangeeventUrl = "https://fakeprovider/" <> changeId
    }

-- | 'nominalMerge' is the most simple scenario
-- >>> let project = SProject "openstack/nova" [alice, bob] [alice] [eve]
-- >>> showEvents $ nominalMerge project "42" now (3600*24)
-- "Change[2021-06-10 change-42 created by eve], Change[change-42], Commented[alice], Merged[2021-06-11], Reviewed[alice]"
nominalMerge :: ScenarioProject -> LText -> UTCTime -> Integer -> [ScenarioEvent]
nominalMerge SProject {..} changeId start duration = evalRand scenario stdGen
 where
  -- The random number generator is based on the name
  stdGen = mkStdGen (Text.length (from name))

  scenario = do
    -- The base change
    let mkChange' ts author =
          (mkChange ts start author changeId name EChangeMerged)
            { echangeDuration = Just . fromInteger $ duration
            }
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
  stdGen = mkStdGen (Text.length (from name))

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
