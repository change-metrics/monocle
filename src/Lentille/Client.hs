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
    TasksSummary (..),

    -- * API
    getIndices,
    getTasksSummary,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier, omitNothingFields), defaultOptions, eitherDecode, genericParseJSON)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, requestHeaders, responseBody)
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
  -- | The callback
  (MonocleClient -> m ()) ->
  -- | withClient performs the IO
  m ()
withClient url callBack =
  do
    manager <- liftIO $ newManager tlsManagerSettings
    callBack MonocleClient {..}
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

newtype APIPath = APIPath Text

monocleGet ::
  (MonadIO m, MonadThrow m, FromJSON a) =>
  APIPath ->
  MonocleClient ->
  m a
monocleGet (APIPath path) MonocleClient {..} =
  do
    initRequest <- parseUrlThrow (T.unpack $ baseUrl <> path)
    let request = initRequest {requestHeaders = [("Accept", "*/*")]}
    response <- liftIO $ httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> show err
      Right a -> pure a

getIndices :: (MonadThrow m, MonadIO m) => MonocleClient -> m [Text]
getIndices = monocleGet (APIPath "api/0/indices")

-- TODO: check actual implementation and add TaskTracker type
data TasksSummary = TasksSummary
  { ts_url :: Text,
    ts_last_updated :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TasksSummary where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 3, omitNothingFields = True})

getTasksSummary :: (MonadThrow m, MonadIO m) => MonocleClient -> m [TasksSummary]
-- getTasksSummary = monocleGet (APIPath "api/0/query/tasks_summary")
getTasksSummary _ = do
  -- mock value
  now <- liftIO getCurrentTime
  pure [TasksSummary "https://github.com" (Just now), TasksSummary "https://bugzilla.redhat.com" Nothing]
