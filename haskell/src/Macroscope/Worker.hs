-- TEMP, to remove when org and task data are migrated to this new system
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker interface.
module Macroscope.Worker
  ( runStream,
    DocumentStream (..),
  )
where

import qualified Data.Text as Text
import qualified Data.Vector as V
import Google.Protobuf.Timestamp as Timestamp
import Lentille
import Monocle.Backend.Index (entityRequestOrganization, entityRequestProject, entityRequestTaskData)
import Monocle.Change (Change, ChangeEvent)
import Monocle.Client (MonocleClient)
import Monocle.Client.Api
import Monocle.Crawler
import Monocle.Prelude
import Monocle.Project (Project)
import Monocle.Search (TaskData)
import qualified Streaming as S
import qualified Streaming.Prelude as S

-- | A crawler is defined as a DocumentStream:
data DocumentStream
  = -- | Fetch project for a organization name
    Projects (Text -> LentilleStream Project)
  | -- | Fetch recent changes from a project
    Changes (UTCTime -> Text -> LentilleStream (Change, [ChangeEvent]))
  | -- | Fetch recent task data
    TaskDatas (UTCTime -> LentilleStream TaskData)

-------------------------------------------------------------------------------
-- Adapter between protobuf api and crawler stream
-------------------------------------------------------------------------------

type ApiKey = LText

type IndexName = LText

type CrawlerName = LText

-- | 'getDate' gets the updated time of an 'OldestEntity'
getDate :: OldestEntity -> UTCTime
getDate oe =
  toUTCTime
    . fromMaybe (error "missing TS")
    $ commitInfoResponse_OldestEntityLastCommitAt oe

-- | 'getProject' gets the updated time and project name from an 'OldestEntity'
getProject :: OldestEntity -> (UTCTime, Text)
getProject oe = (getDate oe, toStrict project)
  where
    project =
      case commitInfoResponse_OldestEntityEntity oe of
        Just (Entity (Just (EntityEntityProjectName name))) -> name
        _ -> error $ "Not a project: " <> show oe

-- | 'getOrganization' gets the updated time and organization name from an 'OldestEntity'
getOrganization :: OldestEntity -> (UTCTime, Text)
getOrganization oe = (getDate oe, toStrict organization)
  where
    organization =
      case commitInfoResponse_OldestEntityEntity oe of
        Just (Entity (Just (EntityEntityOrganizationName name))) -> name
        _ -> error $ "Not an organization: " <> show oe

-------------------------------------------------------------------------------
-- Worker implementation
-------------------------------------------------------------------------------

-- | The crawler stream is (locally) converted to a Stream (Of DocumentType)
-- This intermediary representation enables generic processing with 'processBatch'
data DocumentType
  = DTProject Project
  | DTChanges (Change, [ChangeEvent])
  | DTTaskData TaskData

data ProcessResult = AddOk | AddError Text deriving stock (Show)

type OldestEntity = CommitInfoResponse_OldestEntity

-- | 'processBatch' handles the monocle api crawlerAddDoc call
processBatch :: (MonadIO m, MonadLog m) => ([DocumentType] -> m AddDocResponse) -> [DocumentType] -> m ProcessResult
processBatch postFunc docs = do
  log $ LogPostData (length docs)
  resp <- postFunc docs
  pure $ case resp of
    AddDocResponse Nothing -> AddOk
    AddDocResponse (Just err) -> AddError (show err)

-- | 'process' post to the monocle api a stream of document
process :: (MonadIO m, MonadLog m) => ([DocumentType] -> m AddDocResponse) -> Stream (Of DocumentType) m () -> m [ProcessResult]
process postFunc =
  S.toList_
    . S.mapM (processBatch postFunc)
    . S.mapped S.toList
    . S.chunksOf 500

-- | Run is the main function used by macroscope
runStream ::
  (MonadMask m, MonadRetry m, MonadLog m, MonadIO m) =>
  MonocleClient ->
  UTCTime ->
  ApiKey ->
  IndexName ->
  CrawlerName ->
  DocumentStream ->
  m ()
runStream monocleClient startDate apiKey indexName crawlerName documentStream = drainEntities (0 :: Word32)
  where
    drainEntities offset = do
      -- It is important to get the commit date before starting the process to not miss
      -- document updated when we start
      startTime <- log' $ LogStartingEntity entityType

      -- Query the monocle api for the oldest entity to be updated.
      oldestEntity <- retry $ getOldestEntity offset
      log $ LogOldestEntity oldestEntity

      if oldestEntityDate oldestEntity > startDate
        then log LogEnded
        else do
          -- Run the document stream for that entity
          postResultE <-
            runLentilleM $
              process
                (retry . crawlerAddDoc monocleClient . mkRequest oldestEntity)
                (getStream oldestEntity)

          case postResultE of
            Right postResult ->
              case foldr collectPostFailure [] postResult of
                [] -> do
                  -- Post the commit date
                  res <- retry $ commitTimestamp oldestEntity startTime

                  if not res
                    then log LogFailed
                    else do
                      putTextLn "Continuing..."
                      drainEntities offset
                xs -> do
                  log $ LogNetworkFailure $ "Could not post document: " <> Text.intercalate " | " xs
            Left err -> do
              -- TODO: report decoding error
              putTextLn $ "Lentille error: " <> show err
              log LogFailed
              drainEntities (offset + 1)

    collectPostFailure :: ProcessResult -> [Text] -> [Text]
    collectPostFailure res acc = case res of
      AddOk -> acc
      AddError err -> err : acc

    oldestEntityDate oe = case oe of
      CommitInfoResponse_OldestEntity _ (Just tc) -> Timestamp.toUTCTime tc
      _ -> error "Timestamp missing"

    -- Adapt the document stream to intermediate representation
    getStream oldestEntity = case documentStream of
      Changes s ->
        let (untilDate, project) = getProject oldestEntity
         in S.map DTChanges (s untilDate project)
      Projects s ->
        let (_, organization) = getOrganization oldestEntity
         in S.map DTProject (s organization)
      TaskDatas s ->
        let untilDate = getDate oldestEntity
         in S.map DTTaskData (s untilDate)

    -- The type of the oldest entity for a given document stream
    entityType = case documentStream of
      Projects _ -> entityRequestOrganization
      Changes _ -> entityRequestProject
      TaskDatas _ -> entityRequestTaskData

    getOldestEntity offset = do
      resp <-
        crawlerCommitInfo
          monocleClient
          ( CommitInfoRequest
              indexName
              crawlerName
              (Just $ Entity $ Just entityType)
              offset
          )
      case resp of
        CommitInfoResponse (Just (CommitInfoResponseResultEntity entity)) -> pure entity
        _ -> error $ "Could not get initial timestamp: " <> show resp

    -- 'mkRequest' creates the 'AddDocRequests' for a given oldest entity and a list of documenttype
    -- this is used by the processBatch function.
    mkRequest :: OldestEntity -> [DocumentType] -> AddDocRequest
    mkRequest oe xs =
      let addDocRequestIndex = indexName
          addDocRequestCrawler = crawlerName
          addDocRequestApikey = apiKey
          addDocRequestEntity = commitInfoResponse_OldestEntityEntity oe
          addDocRequestChanges = V.fromList $ mapMaybe getChanges xs
          addDocRequestEvents = V.fromList $ concat $ mapMaybe getEvents xs
          addDocRequestProjects = V.fromList $ mapMaybe getProject' xs
          addDocRequestTaskDatas = V.fromList $ mapMaybe getTD xs
       in AddDocRequest {..}
      where
        getEvents = \case
          DTChanges (_, events) -> Just events
          _ -> Nothing
        getChanges = \case
          DTChanges (change, _) -> Just change
          _ -> Nothing
        getProject' = \case
          DTProject p -> Just p
          _ -> Nothing
        getTD = \case
          DTTaskData td -> Just td
          _ -> Nothing

    -- 'commitTimestamp' post the commit date.
    commitTimestamp oe startTime = do
      commitResp <-
        crawlerCommit
          monocleClient
          ( CommitRequest
              indexName
              crawlerName
              apiKey
              (commitInfoResponse_OldestEntityEntity oe)
              (Just $ Timestamp.fromUTCTime startTime)
          )
      case commitResp of
        (CommitResponse (Just (CommitResponseResultTimestamp _))) -> pure True
        (CommitResponse (Just (CommitResponseResultError err))) -> do
          putTextLn ("Commit failed: " <> show err)
          pure False
        _ -> do
          putTextLn "Empty commit response"
          pure False
