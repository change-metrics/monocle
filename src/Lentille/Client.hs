{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle API client
module Lentille.Client
  ( -- * Client
    MonocleClient,
    withClient,

    -- * Data types
    TrackerData (..),

    -- * Query type
    IndexName (..),
    CrawlerName (..),
    ApiKey (..),

    -- * API
    getIndices,
    getUpdatedSince,
    postTrackerData,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client
  ( Manager,
    Request,
    RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseUrlThrow,
    requestBody,
    requestHeaders,
    responseBody,
    setQueryString,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude

-- | The MonocleClient record, use 'withClient' to create
data MonocleClient = MonocleClient
  { -- | the base url
    baseUrl :: Text,
    manager :: Manager
  }

-- | Create the 'MonocleClient'
withClient ::
  MonadIO m =>
  -- | The monocle api url
  Text ->
  -- | An optional manager
  Maybe Manager ->
  -- | The callback
  (MonocleClient -> m ()) ->
  -- | withClient performs the IO
  m ()
withClient url managerM callBack =
  do
    manager <- case managerM of
      Just manager' -> pure manager'
      Nothing -> liftIO $ newManager tlsManagerSettings
    callBack MonocleClient {..}
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

newtype APIPath = APIPath Text

newtype Verb = Verb ByteString

type QS = [(ByteString, Maybe ByteString)]

monocleReq ::
  (MonadIO m, MonadThrow m, ToJSON body, FromJSON a) =>
  Verb ->
  Maybe body ->
  APIPath ->
  QS ->
  MonocleClient ->
  m a
monocleReq (Verb verb) bodyM (APIPath path) qs MonocleClient {..} =
  do
    initRequest <- parseUrlThrow (T.unpack $ baseUrl <> path)
    let request =
          withBody . withQs $
            initRequest
              { requestHeaders = [("Accept", "*/*")],
                method = verb
              }
    response <- liftIO $ httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> show err
      Right a -> pure a
  where
    withBody :: Request -> Request
    withBody = case bodyM of
      Nothing -> id
      Just body -> \r ->
        r
          { requestBody = RequestBodyLBS . encode $ body,
            requestHeaders = requestHeaders r <> [("Content-Type", "application/json")]
          }
    withQs :: Request -> Request
    withQs = case qs of
      [] -> id
      xs -> setQueryString xs

monocleGet :: (MonadIO m, MonadThrow m, FromJSON a) => APIPath -> QS -> MonocleClient -> m a
monocleGet = monocleReq (Verb "GET") (Nothing :: Maybe Value)

monoclePost :: (MonadIO m, MonadThrow m, ToJSON a) => Maybe a -> APIPath -> QS -> MonocleClient -> m ()
monoclePost = monocleReq (Verb "POST")

getIndices :: (MonadThrow m, MonadIO m) => MonocleClient -> m [Text]
getIndices = monocleGet (APIPath "api/0/indices") mempty

newtype IndexName = IndexName Text

newtype CrawlerName = CrawlerName Text

newtype ApiKey = ApiKey Text

-- fmap . fmap  let you go through 2 functors!
mkQs :: [(ByteString, Text)] -> [(ByteString, Maybe ByteString)]
mkQs = fmap . fmap $ Just . encodeUtf8

getUpdatedSince :: (MonadThrow m, MonadIO m) => MonocleClient -> IndexName -> CrawlerName -> m UTCTime
getUpdatedSince client (IndexName index) (CrawlerName crawler) =
  monocleGet (APIPath "api/0/task_tracker/updated_since_date") qs client
  where
    qs = mkQs [("index", index), ("name", crawler)]

data TrackerData = TrackerData
  { tdUpdatedAt :: UTCTime,
    tdChangeUrl :: Text,
    tdIssueUrl :: Text,
    tdIssueTitle :: Text,
    tdIssueId :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TrackerData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

postTrackerData ::
  (MonadThrow m, MonadIO m) =>
  MonocleClient ->
  IndexName ->
  CrawlerName ->
  ApiKey ->
  [TrackerData] ->
  m ()
postTrackerData client (IndexName index) (CrawlerName crawler) (ApiKey apikey) tds = do
  monoclePost (Just tds) (APIPath "api/0/amend/tracker_data") qs client
  where
    qs = mkQs [("index", index), ("name", crawler), ("apikey", apikey)]
