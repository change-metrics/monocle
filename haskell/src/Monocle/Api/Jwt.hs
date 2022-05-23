module Monocle.Api.Jwt
  ( mkMagicJwt,
  )
where

import Crypto.JWT
  ( ClaimsSet,
    JWK,
    JWTError,
    KeyMaterialGenParam (RSAGenParam),
    NumericDate (NumericDate),
    SignedJWT,
    StringOrURI,
    bestJWSAlg,
    claimIat,
    claimIss,
    claimSub,
    emptyClaimsSet,
    genJWK,
    newJWSHeader,
    signClaims,
  )
import Monocle.Prelude
  ( getCurrentTime,
    (?~),
  )
import Relude

mkMagicClaims :: MonadIO m => StringOrURI -> m ClaimsSet
mkMagicClaims subject = do
  t <- getCurrentTime
  pure $
    emptyClaimsSet
      & claimIss ?~ "MagicJWT"
      & claimSub ?~ subject
      & claimIat ?~ NumericDate t

doSignJwt :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doSignJwt jwk claims = runExceptT $ do
  alg <- bestJWSAlg jwk
  signClaims jwk (newJWSHeader ((), alg)) claims

doGenJwk :: IO JWK
doGenJwk = genJWK (RSAGenParam (4096 `div` 8))

mkMagicJwt :: String -> IO (Either JWTError SignedJWT)
mkMagicJwt subject = do
  claims <- mkMagicClaims $ fromString subject
  jwk <- doGenJwk
  doSignJwt jwk claims

-- TODO: Implement a way to read jwks_uri to fetch provider pub key
-- https://accounts.google.com/.well-known/openid-configuration