module Monocle.Api.Jwt
  ( --- MagicJWT
    mkMagicJwt,
    doGenJwk,
    AuthenticatedUser (..),
    --- OIDC Flow
    OIDCEnv (..),
    AuthInfo (..),
    User (..),
    LoginHandler,
    handleOIDCLogin,
    initOIDC,
  )
where

import Crypto.JWT
  ( JWK,
    KeyMaterialGenParam (RSAGenParam),
    genJWK,
  )
import Crypto.JWT qualified as Jose
import Data.Aeson ((.:))
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as AeT
import Data.ByteString.Lazy qualified as BSL
import Monocle.Config (OIDCProvider (..))
import Monocle.Prelude
  ( FromJSON (parseJSON),
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

newtype JWKS = JWKS {keys :: [Jose.RSAKeyParameters]} deriving (Generic, Show)

instance FromJSON JWKS

newtype OIDCConfig = OIDCConfig {jwks_uri :: Text} deriving (Generic, Show)

instance FromJSON OIDCConfig

--- $ OIDC Flow

data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC,
    mgr :: Manager,
    genState :: IO ByteString,
    prov :: O.Provider,
    redirectUri :: ByteString,
    clientId :: ByteString,
    clientPassword :: ByteString
  }

data AuthInfo = AuthInfo
  { email :: Text,
    emailVerified :: Bool,
    name :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON AuthInfo where
  parseJSON (JSON.Object v) = do
    email :: Text <- v .: "email"
    email_verified :: Bool <- v .: "email_verified"
    name :: Text <- v .: "name"
    return $ AuthInfo (from email) email_verified (from name)
  parseJSON invalid = AeT.typeMismatch "Coord" invalid

instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n) =
    JSON.object
      [ "email" JSON..= (from e :: Text),
        "email_verified" JSON..= ev,
        "name" JSON..= (from n :: Text)
      ]

data User = User
  { userId :: Text,
    userSecret :: Text,
    localStorageKey :: Text,
    redirectUrl :: Maybe Text
  }
  deriving (Show, Eq, Ord)

type APIKey = ByteString

type Account = Text

data Customer = Customer
  { account :: Account,
    apiKey :: APIKey,
    mail :: Maybe Text,
    fullname :: Maybe Text
  }

type LoginHandler = AuthInfo -> IO (Either Text User)

customerFromAuthInfo :: AuthInfo -> IO Customer
customerFromAuthInfo authinfo = do
  apikey <- genRandomBS
  return
    Customer
      { account = from (email authinfo),
        apiKey = apikey,
        mail = Just (from (email authinfo)),
        fullname = Just (from (name authinfo))
      }

handleOIDCLogin :: LoginHandler
handleOIDCLogin authInfo = do
  custInfo <- customerFromAuthInfo authInfo
  if emailVerified authInfo
    then return . Right . customerToUser $ custInfo
    else return (Left "You emails is not verified by your provider. Please verify your email.")
  where
    customerToUser :: Customer -> User
    customerToUser c =
      User
        { userId = from (account c),
          userSecret = decodeUtf8 (apiKey c),
          redirectUrl = Nothing,
          localStorageKey = "api-key"
        }

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

initOIDC :: OIDCProvider -> String -> IO OIDCEnv
initOIDC OIDCProvider {..} clientSecret' = do
  mgr <- newOpenSSLManager
  prov <- O.discover issuer mgr
  let publicUrl = "http://localhost:8080" -- TODO "Discover it or add it in config"
      redirectUri = publicUrl <> "/api/2/auth/cb"
      clientId = from client_id
      clientSecret = from clientSecret'
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC prov)
      genState = genRandomBS
      clientPassword = clientSecret
  pure OIDCEnv {..}