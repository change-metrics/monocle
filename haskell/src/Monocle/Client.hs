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
    retry,
  )
where

import Control.Monad.Catch (Handler (Handler), MonadThrow)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import qualified Data.Text as T
import Monocle.Client.Worker
import qualified Network.Connection as Connection
import Network.HTTP.Client
  ( HttpException (..),
    Manager,
    RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseUrlThrow,
    requestBody,
    requestHeaders,
    responseBody,
  )
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Proto3.Suite.JSONPB (FromJSONPB (..), ToJSONPB (..))
import qualified Proto3.Suite.JSONPB as JSONPB
import Relude

-- | The MonocleClient record, use 'withClient' to create
data MonocleClient = MonocleClient
  { -- | the base url
    baseUrl :: Text,
    manager :: Manager
  }

-- | Create a HTTP manager
mkManager :: MonadIO m => m Manager
mkManager = do
  disableTlsM <- lookupEnv "TLS_NO_VERIFY"
  let managerSettings = case disableTlsM of
        Just _ ->
          let tlsSettings = Connection.TLSSettingsSimple True False False
           in HTTP.mkManagerSettings tlsSettings Nothing
        Nothing -> HTTP.tlsManagerSettings
  liftIO $ newManager managerSettings

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
      Nothing -> mkManager
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

-- Retry 5 times network action, doubling backoff each time
retry :: (MonadMask m, MonadLog m, MonadIO m) => m a -> m a
retry action =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 6)
    [handler]
    (const action)
  where
    backoff = 500000 -- 500ms
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        log . LogNetworkFailure $ show num <> "/6 " <> loc <> " failed: " <> show ctx
        pure True
      InvalidUrlException _ _ -> pure False
