module Monocle.Api.Jwt
  ( --- JWT
    mkJwt,
    doGenJwk,
    AuthenticatedUser (..),
    --- OIDC Flow
    OIDCEnv (..),
    LoginInUser (..),
    initOIDCEnv,
    mkSessionStore,
    OIDCState (OIDCState),
    decodeOIDCState,
  )
where

import Control.Monad.Random (genByteString)
import Control.Monad.Random qualified as Random
import Crypto.Hash.SHA256 (hash)
import Crypto.JWT (Error, JWK, fromOctets)
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as HM
import Monocle.Config (OIDCProviderConfig (..))
import Monocle.Prelude
import Network.HTTP.Client (Manager)
import Servant.Auth.Server
  ( FromJWT,
    JWTSettings,
    ToJWT,
    makeJWT,
  )
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html5 qualified as H
import Web.OIDC.Client qualified as O

--- * JWT handling

doGenJwk :: Maybe ByteString -> IO JWK
doGenJwk keyM = case keyM of
  Just key | BS.length key >= 64 -> pure $ keyFromBS key
  _ -> randomJWK
  where
    randomJWK = keyFromBS <$> genRandom
    keyFromBS = fromOctets . take 64 . BSL.unpack . from

type MUidMap = Map Text Text

-- Will be added as the 'dat' unregistered claim
data AuthenticatedUser = AUser
  { -- A mapping that contains a Monocle UID by Index name
    aMuidMap :: MUidMap,
    -- A default Monocle UID to be used when aMuidMap is empty
    aDefaultMuid :: Text
  }
  deriving (Generic, Show)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

mkJwt :: JWTSettings -> MUidMap -> Text -> Maybe UTCTime -> IO (Either Error BSL.ByteString)
mkJwt settings aMuidMap aDefaultMuid = makeJWT (AUser {..}) settings

--- $ OIDC Flow

data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC,
    manager :: Manager,
    provider :: O.Provider,
    redirectUri :: ByteString,
    sessionStoreStorage :: MVar (HM.Map O.State O.Nonce),
    providerConfig :: OIDCProviderConfig
  }

data LoginInUser = LoginInUser
  { liJWT :: Text,
    liRedirectURI :: Text
  }
  deriving (Show, Eq, Ord)

instance ToMarkup LoginInUser where
  toMarkup LoginInUser {..} = H.docTypeHtml $ do
    H.head $
      H.title "Redirecting after succesfull login ..."
    H.body $ do
      H.script
        ( H.toHtml
            ( "localStorage.setItem('api-key','" <> liJWT <> "');"
                <> "window.location='"
                <> liRedirectURI
                <> "';"
            )
        )

initOIDCEnv :: OIDCProviderConfig -> IO OIDCEnv
initOIDCEnv providerConfig@OIDCProviderConfig {..} = do
  manager <- newOpenSSLManager
  provider <- O.discover opIssuerURL manager
  sessionStoreStorage <- newMVar HM.empty
  let redirectUri = from $ opAppPublicURL <> "api/2/auth/cb"
      clientId = from opClientID
      clientSecret = from opClientSecret
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
  pure OIDCEnv {..}

data OIDCState = OIDCState
  { randomT :: Text,
    uri :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON OIDCState

instance ToJSON OIDCState

decodeOIDCState :: ByteString -> Maybe OIDCState
decodeOIDCState bs = case B64.decode bs of
  Right json -> decode $ from json
  Left _ -> Nothing

mkSessionStore :: OIDCEnv -> Maybe O.State -> Maybe Text -> O.SessionStore IO
mkSessionStore OIDCEnv {sessionStoreStorage} stateM uriM = do
  let sessionStoreGenerate = do
        rb <- liftIO genRandomB64
        let s = OIDCState (decodeUtf8 rb) uriM
        pure (B64.encode $ BSL.toStrict $ encode s)
      sessionStoreSave = storeSave
      sessionStoreGet = storeGet
      sessionStoreDelete = case stateM of
        Just state' -> modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.delete state' store
        Nothing -> pure ()
   in O.SessionStore {..}
  where
    storeSave :: O.State -> O.Nonce -> IO ()
    storeSave state' nonce = modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.insert state' nonce store
    storeGet :: IO (Maybe O.State, Maybe O.Nonce)
    storeGet = case stateM of
      Just state' -> do
        store <- readMVar sessionStoreStorage
        let nonce = HM.lookup state' store
        pure (Just state', nonce)
      Nothing -> pure (Nothing, Nothing)

-- | Generate a random fixed size string of 42 char base64 encoded
genRandomB64 :: IO ByteString
genRandomB64 = B64.encode . hash <$> genRandom

-- | Generate a random fixed size string of 1024 Bytes
genRandom :: IO ByteString
genRandom = do
  g <- Random.newStdGen
  let (bs, _ng) = genByteString 1024 g
  pure bs
