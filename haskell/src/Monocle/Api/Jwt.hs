module Monocle.Api.Jwt
  ( --- JWT
    mkJwt,
    doGenJwk,
    AuthenticatedUser (..),
    --- OIDC Flow
    OIDCEnv (..),
    LoginInUser (..),
    initOIDCEnv,
  )
where

import Crypto.JWT (Error, JWK, KeyMaterialGenParam (RSAGenParam), genJWK)
import Data.ByteString.Lazy qualified as BSL
import Monocle.Config (OIDCProvider (..))
import Monocle.Prelude (FromJSON, ToJSON, UTCTime, from, newOpenSSLManager)
import Network.HTTP.Client (Manager)
import Relude
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
    clientSecret :: ByteString
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

initOIDCEnv :: OIDCProvider -> String -> IO OIDCEnv
initOIDCEnv OIDCProvider {..} clientSecret' = do
  manager <- newOpenSSLManager
  provider <- O.discover issuer manager
  let publicUrl = "http://localhost:8080" -- TODO "Discover it or add it in config"
      redirectUri = publicUrl <> "/api/2/auth/cb"
      clientId = from client_id
      clientSecret = from clientSecret'
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
  pure OIDCEnv {..}
