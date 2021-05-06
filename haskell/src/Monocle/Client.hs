{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle API client
module Monocle.Client
  ( MonocleClient,
    withClient,

    -- * API
    getLastUpdated,
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Text as T
import Monocle.TaskData
import Network.HTTP.Client
  ( Manager,
    RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseUrlThrow,
    requestBody,
    requestHeaders,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Proto3.Suite.JSONPB (FromJSONPB (..), ToJSONPB (..))
import qualified Proto3.Suite.JSONPB as JSONPB
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
  (MonocleClient -> m a) ->
  -- | withClient performs the IO
  m a
withClient url managerM callBack =
  do
    manager <- case managerM of
      Just manager' -> pure manager'
      Nothing -> liftIO $ newManager tlsManagerSettings
    callBack MonocleClient {..}
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

newtype APIPath = APIPath Text

monocleReq ::
  (MonadIO m, MonadThrow m, Show body, ToJSONPB body, FromJSONPB a) =>
  MonocleClient ->
  APIPath ->
  body ->
  m a
monocleReq MonocleClient {..} (APIPath path) body =
  do
    initRequest <- parseUrlThrow (T.unpack $ baseUrl <> path)
    let request =
          initRequest
            { requestHeaders = [("Accept", "*/*"), ("Content-Type", "application/json")],
              method = "POST",
              requestBody = RequestBodyLBS . JSONPB.encode JSONPB.jsonPBOptions $ body
            }
    response <- liftIO $ httpLbs request manager
    pure $ decodeResponse (responseBody response)
  where
    decodeResponse body' = case JSONPB.eitherDecode body' of
      Left err -> error $ "Decoding of " <> show body <> " failed with: " <> show err
      Right a -> a

getLastUpdated ::
  (MonadThrow m, MonadIO m) =>
  MonocleClient ->
  TaskDataGetLastUpdatedRequest ->
  m TaskDataGetLastUpdatedResponse
getLastUpdated client req =
  monocleReq client (APIPath "api/1/task_data_get_last_updated") req
