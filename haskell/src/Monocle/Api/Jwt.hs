module Monocle.Api.Jwt
  ( --- MagicJWT
    mkMagicJwt,
    doGenJwk,
    AuthenticatedUser (..),
    --- OIDC Flow
    OIDCEnv (..),
    User (..),
    initOIDCEnv,
  )
where

import Crypto.JWT
  ( JWK,
    KeyMaterialGenParam (RSAGenParam),
    genJWK,
  )
import Crypto.JWT qualified as Jose
import Data.ByteString.Lazy qualified as BSL
import Monocle.Config (OIDCProvider (..))
import Monocle.Prelude
  ( FromJSON,
    ToJSON,
    from,
    genRandomBS,
  )
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.OpenSSL (newOpenSSLManager)
import Relude
import Servant.Auth.Server
  ( FromJWT,
    JWTSettings,
    ToJWT,
    makeJWT,
  )
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Web.OIDC.Client qualified as O

--- * MagicJWT handling

doGenJwk :: IO JWK
doGenJwk = genJWK (RSAGenParam (4096 `div` 8))

-- Will be added as the 'dat' unregistered claim
newtype AuthenticatedUser = AUser {aMuid :: Text} deriving (Generic, Show)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

mkMagicJwt :: JWTSettings -> Text -> IO (Either Jose.Error BSL.ByteString)
mkMagicJwt settings muid = let expD = Nothing in makeJWT (AUser muid) settings expD

--- $ OIDC Flow

data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC,
    manager :: Manager,
    genState :: IO ByteString,
    provider :: O.Provider,
    redirectUri :: ByteString,
    clientId :: ByteString,
    clientSecret :: ByteString
  }

data User = User
  { userId :: Text,
    userSecret :: Text,
    localStorageKey :: Text,
    redirectUrl :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToMarkup User where
  toMarkup User {..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged In"
      H.p (H.toHtml ("Successful login with id " <> userId))
      H.script
        ( H.toHtml
            ( "localStorage.setItem('" <> localStorageKey <> "','" <> userSecret <> "');"
                <> "localStorage.setItem('user-id','"
                <> userId
                <> "');"
                <> "window.location='"
                <> fromMaybe "/" redirectUrl
                <> "';" -- redirect the user to /
            )
        )

initOIDCEnv :: OIDCProvider -> String -> IO OIDCEnv
initOIDCEnv OIDCProvider {..} clientSecret' = do
  manager <- newOpenSSLManager
  provider <- O.discover issuer manager
  let publicUrl = "http://localhost:8080" -- TODO "Discover it or add it in config"
      redirectUri = publicUrl <> "/api/2/auth/cb"
      clientId = from client_id
      clientSecret = from clientSecret'
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
      genState = genRandomBS
  pure OIDCEnv {..}
