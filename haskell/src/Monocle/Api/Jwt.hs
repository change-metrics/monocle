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
  )
where

import Control.Monad.Random (genByteString)
import Control.Monad.Random qualified as Random
import Crypto.Hash.SHA256 (hash)
import Crypto.JWT (Error, JWK, KeyMaterialGenParam (RSAGenParam), genJWK)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as HM
import Monocle.Config (OIDCProvider (..))
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

doGenJwk :: IO JWK
doGenJwk = genJWK (RSAGenParam (4096 `div` 8))

-- Will be added as the 'dat' unregistered claim
data AuthenticatedUser = AUser
  { aMuid :: Text,
    aAliases :: [Text],
    aGroups :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

mkJwt :: JWTSettings -> Text -> Maybe UTCTime -> IO (Either Error BSL.ByteString)
mkJwt settings muid expD =
  let aMuid = muid
      aAliases = mempty
      aGroups = mempty
   in makeJWT (AUser {..}) settings expD

--- $ OIDC Flow

data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC,
    manager :: Manager,
    provider :: O.Provider,
    redirectUri :: ByteString,
    clientId :: ByteString,
    clientSecret :: ByteString,
    sessionStoreStorage :: MVar (HM.Map O.State O.Nonce),
    userClaim :: Maybe Text
  }

newtype LoginInUser = LoginInUser {liJWT :: Text} deriving (Show, Eq, Ord)

instance ToMarkup LoginInUser where
  toMarkup LoginInUser {..} = H.docTypeHtml $ do
    H.head $
      H.title "Redirecting after succesfull login ..."
    H.body $ do
      H.script
        ( H.toHtml
            ( "localStorage.setItem('api-key','" <> liJWT <> "');"
                <> "window.location='/';"
            )
        )

initOIDCEnv :: OIDCProvider -> IO OIDCEnv
initOIDCEnv OIDCProvider {..} = do
  manager <- newOpenSSLManager
  provider <- O.discover opIssuerURL manager
  sst <- newMVar HM.empty
  let redirectUri = from $ opAppPublicURL <> "api/2/auth/cb"
      clientId = from opClientID
      clientSecret = from opClientSecret
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
      sessionStoreStorage = sst
      userClaim = opUserClaim
  pure OIDCEnv {..}

mkSessionStore :: OIDCEnv -> Maybe O.State -> O.SessionStore IO
mkSessionStore OIDCEnv {sessionStoreStorage} stateM = do
  let sessionStoreGenerate = liftIO genRandom
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

genRandom :: IO ByteString
genRandom = do
  g <- Random.newStdGen
  let (bs, _ng) = genByteString 42 g
  pure . B64.encode $ hash bs
