{-# LANGUAGE DeriveGeneric #-}
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
module Lentille.Client
  ( -- * Client
    MonocleClient,
    withClient,

    -- * Data types
    TaskData (..),
    IsoTime (..),

    -- * Query type
    IndexName (..),
    CrawlerName (..),
    ApiKey (..),

    -- * API
    getIndices,
    getUpdatedSince,
    postTaskData,
    setUpdatedSince,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), eitherDecode, encode, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
    pure $ decodeResponse (responseBody response)
  where
    decodeResponse body = case eitherDecode body of
      Left err -> error $ "Decoding of " <> show body <> " failed with: " <> show err
      Right a -> a
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

monoclePost :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON resp) => Maybe a -> APIPath -> QS -> MonocleClient -> m resp
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
  monocleGet (APIPath "api/0/task_data") qs client
  where
    qs = mkQs [("index", index), ("name", crawler)]

setUpdatedSince :: (MonadThrow m, MonadIO m) => MonocleClient -> IndexName -> CrawlerName -> ApiKey -> UTCTime -> m Bool
setUpdatedSince client (IndexName index) (CrawlerName crawler) (ApiKey apikey) ts = do
  isOk <$> monoclePost (Just . IsoTime $ ts) (APIPath "api/0/task_data/commit") qs client
  where
    isOk :: Text -> Bool
    isOk = (==) "Commited"
    qs = mkQs [("index", index), ("name", crawler), ("apikey", apikey)]

newtype IsoTime = IsoTime UTCTime deriving stock (Show, Eq)

instance ToJSON IsoTime where
  toJSON (IsoTime utcTime) = String . toText . formatTime defaultTimeLocale "%FT%TZ" $ utcTime

data TaskData = TaskData
  { tdUpdatedAt :: IsoTime,
    tdChangeUrl :: Text,
    tdTtype :: [Text],
    tdTid :: Text,
    tdUrl :: Text,
    tdTitle :: Text,
    tdSeverity :: Text,
    tdPriority :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TaskData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

postTaskData ::
  forall m.
  (MonadThrow m, MonadIO m) =>
  MonocleClient ->
  IndexName ->
  CrawlerName ->
  ApiKey ->
  [TaskData] ->
  m [Text]
postTaskData client (IndexName index) (CrawlerName crawler) (ApiKey apikey) tds = do
  fmap (decodeUtf8 . encode) <$> (monoclePost (Just tds) (APIPath "api/0/task_data") qs client :: m [Value])
  where
    qs = mkQs [("index", index), ("name", crawler), ("apikey", apikey)]
