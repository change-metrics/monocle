module Monocle.Api.Jwt
  ( mkMagicJwt,
    doGenJwk,
    AuthenticatedUser (..),
    getOIDCProviderPublicKeys,
  )
where

import Crypto.JWT
  ( JWK,
    KeyMaterial (RSAKeyMaterial),
    KeyMaterialGenParam (RSAGenParam),
    fromKeyMaterial,
    genJWK,
  )
import Crypto.JWT qualified as Jose
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BSL
import Monocle.Prelude
  ( FromJSON,
    MonadThrow,
    ToJSON,
    from,
  )
import Network.HTTP.Client
  ( Manager,
    Request (method, requestHeaders),
    Response (responseBody),
    httpLbs,
    parseUrlThrow,
  )
import Network.HTTP.Client.OpenSSL (newOpenSSLManager)
import Relude
import Servant.Auth.Server
  ( FromJWT,
    JWTSettings,
    ToJWT,
    makeJWT,
  )

-- mkMagicClaims :: MonadIO m => StringOrURI -> m ClaimsSet
-- mkMagicClaims subject = do
--   t <- getCurrentTime
--   pure $
--     emptyClaimsSet
--       & claimIss ?~ "MagicJWT"
--       & claimSub ?~ subject
--       & claimIat ?~ NumericDate t

-- doSignJwt :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
-- doSignJwt jwk claims = runExceptT $ do
--   alg <- bestJWSAlg jwk
--   signClaims jwk (newJWSHeader ((), alg)) claims

-- mkMagicJwt' :: String -> JWK -> IO (Either JWTError SignedJWT)
-- mkMagicJwt' subject jwk = do
--   claims <- mkMagicClaims $ fromString subject
--   doSignJwt jwk claims

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

-- getOIDCProviderPublicKeys "https://accounts.google.com/.well-known/openid-configuration"
getOIDCProviderPublicKeys :: (MonadThrow m, MonadIO m) => Text -> m [JWK]
getOIDCProviderPublicKeys url = do
  manager <- newOpenSSLManager
  configM <- getJwksFromOIDCConfig manager url
  case configM of
    Just config -> do
      jwksM <- getRemoteJwks manager config
      pure $ fromKeyMaterial . RSAKeyMaterial <$> maybe [] keys jwksM
    Nothing -> pure []
  where
    performGET :: (MonadIO m, MonadThrow m, FromJSON r) => Manager -> Text -> m (Maybe r)
    performGET manager url' = do
      initRequest <- parseUrlThrow $ from url'
      let request =
            initRequest
              { requestHeaders = [("Accept", "application/json")],
                method = "GET"
              }
      response <- liftIO $ httpLbs request manager
      pure $ decode (responseBody response)

    getRemoteJwks :: (MonadThrow m, MonadIO m) => Manager -> OIDCConfig -> m (Maybe JWKS)
    getRemoteJwks manager OIDCConfig {..} = do
      performGET manager $ from jwks_uri

    getJwksFromOIDCConfig :: (MonadThrow m, MonadIO m) => Manager -> Text -> m (Maybe OIDCConfig)
    getJwksFromOIDCConfig = performGET
