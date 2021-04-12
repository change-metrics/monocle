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
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, requestHeaders, responseBody, setQueryString)
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

type QS = [(ByteString, Maybe ByteString)]

monocleGet ::
  (MonadIO m, MonadThrow m, FromJSON a) =>
  APIPath ->
  QS ->
  MonocleClient ->
  m a
monocleGet (APIPath path) qs MonocleClient {..} =
  do
    initRequest <- parseUrlThrow (T.unpack $ baseUrl <> path)
    let request = withQs $ initRequest {requestHeaders = [("Accept", "*/*")]}
    response <- liftIO $ httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> show err
      Right a -> pure a
  where
    withQs = case qs of
      [] -> id
      xs -> setQueryString xs

getIndices :: (MonadThrow m, MonadIO m) => MonocleClient -> m [Text]
getIndices = monocleGet (APIPath "api/0/indices") mempty

newtype IndexName = IndexName Text

newtype CrawlerName = CrawlerName Text

newtype ApiKey = ApiKey Text

getUpdatedSince :: (MonadThrow m, MonadIO m) => MonocleClient -> IndexName -> CrawlerName -> m UTCTime
getUpdatedSince client (IndexName index) (CrawlerName crawler) =
  monocleGet (APIPath "api/0/task_tracker/updated_since_date") qs client
  where
    qs =
      (fmap . fmap $ Just . encodeUtf8)
        [ ("index", index),
          ("name", crawler)
        ]

{-  -- fmap . fmap  let you go through 2 functors!
    qsUpdate :: (Functor l, Functor t) => l (t Text) -> l (t (Maybe ByteString))
    qsUpdate = fmap . fmap $ Just . encodeUtf8
    qsBase :: [(ByteString, Text)]
    qsBase = [("index", index), ("name", crawler)]
-}

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
