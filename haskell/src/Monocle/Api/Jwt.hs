module Monocle.Api.Jwt
  ( mkMagicJwt,
    doGenJwk,
    AuthenticatedUser (..),
  )
where

import Crypto.JWT
  ( JWK,
    KeyMaterialGenParam (RSAGenParam),
    genJWK,
  )
import Crypto.JWT qualified as Jose
import Data.ByteString.Lazy qualified as BSL
import Monocle.Prelude
  ( FromJSON,
    ToJSON,
  )
import Relude
import Servant.Auth.Server (FromJWT, JWTSettings, ToJWT, makeJWT)

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

-- TODO: Implement a way to read jwks_uri to fetch provider pub key
-- https://accounts.google.com/.well-known/openid-configuration
