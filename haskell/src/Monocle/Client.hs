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
    tokenM,
  )
where

import Data.Text qualified as T
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
import Network.HTTP.Client.OpenSSL qualified as OpenSSL
import OpenSSL.Session (VerificationMode (VerifyNone))
import Proto3.Suite.JSONPB (FromJSONPB (..), ToJSONPB (..))
import Proto3.Suite.JSONPB qualified as JSONPB

-- | The MonocleClient record, use 'withClient' to create
data MonocleClient = MonocleClient
  { -- | the base url
    baseUrl :: Text,
    tokenM :: Maybe Text,
    manager :: Manager
  }

data TlsVerify = Verify | Insecure

lookupTlsVerify :: IO TlsVerify
lookupTlsVerify = do
  disableTls <- lookupEnv "TLS_NO_VERIFY"
  pure $ case disableTls of
    Just x | x /= "0" -> Insecure
    _ -> Verify

-- | Create a HTTP manager
mkManager :: IO Manager
mkManager = mkManager' =<< lookupTlsVerify

mkManager' :: TlsVerify -> IO Manager
mkManager' verify = do
  let opensslSettings = case verify of
        Insecure -> OpenSSL.defaultOpenSSLSettings {OpenSSL.osslSettingsVerifyMode = VerifyNone}
        Verify -> OpenSSL.defaultOpenSSLSettings
  tlsCiphers <- fromMaybe "DEFAULT" <$> lookupEnv "TLS_CIPHERS"
  ctx <- OpenSSL.defaultMakeContext (opensslSettings {OpenSSL.osslSettingsCiphers = tlsCiphers})
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
    tokenM' <- liftIO $ lookupEnv "ADMIN_TOKEN"
    let tokenM = from <$> tokenM'
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
            { requestHeaders =
                [ ("Accept", "*/*"),
                  ("Content-Type", "application/json")
                ]
                  <> maybeToList authorizationH,
              method = "POST",
              requestBody = RequestBodyLBS . JSONPB.encode JSONPB.jsonPBOptions $ body
            }
    response <- liftIO $ httpLbs request manager
    pure $ decodeResponse (responseBody response)
  where
    decodeResponse body' = case JSONPB.eitherDecode body' of
      Left err -> error $ "Decoding of " <> show body <> " failed with: " <> show err
      Right a -> a
    authorizationH = fmap (\token -> ("Authorization", "Bearer " <> from token)) tokenM
