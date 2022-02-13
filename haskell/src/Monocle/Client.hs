-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle API client
module Monocle.Client
  ( MonocleClient,
    withClient,
    mkManager,
    monocleReq,
    baseUrl,
  )
where

import qualified Data.Text as T
import Monocle.Prelude
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
import qualified Network.HTTP.Client.OpenSSL as OpenSSL
import OpenSSL.Session (VerificationMode (VerifyNone))
import Proto3.Suite.JSONPB (FromJSONPB (..), ToJSONPB (..))
import qualified Proto3.Suite.JSONPB as JSONPB

-- | The MonocleClient record, use 'withClient' to create
data MonocleClient = MonocleClient
  { -- | the base url
    baseUrl :: Text,
    manager :: Manager
  }

-- | Create a HTTP manager
mkManager :: IO Manager
mkManager = do
  disableTlsM <- lookupEnv "TLS_NO_VERIFY"
  let opensslSettings = case disableTlsM of
        Just _ -> OpenSSL.defaultOpenSSLSettings {OpenSSL.osslSettingsVerifyMode = VerifyNone}
        Nothing -> OpenSSL.defaultOpenSSLSettings
  ctx <- OpenSSL.defaultMakeContext opensslSettings
  newManager $ OpenSSL.opensslManagerSettings (pure ctx)

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
    manager <- maybe (liftIO mkManager) pure managerM
    callBack MonocleClient {..}
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

monocleReq ::
  (MonadIO m, MonadThrow m, Show body, ToJSONPB body, FromJSONPB a) =>
  Text ->
  MonocleClient ->
  body ->
  m a
monocleReq path MonocleClient {..} body =
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
