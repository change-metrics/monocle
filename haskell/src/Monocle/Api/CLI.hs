{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.CLI (run) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import qualified Monocle.Api.Config as Config
import qualified Monocle.Backend.Index as I
import Monocle.Servant.Env
import Monocle.Servant.HTTP (MonocleAPI, server)
import Network.HTTP.Types.Status (seeOther303)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (withStdoutLogger)
import qualified Network.Wai.Middleware.Auth as Auth
import qualified Network.Wai.Middleware.Auth.OAuth2.Github as Auth
import qualified Network.Wai.Middleware.Auth.Provider as Auth
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Relude
import Servant (hoistServer, serve)

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: Env -> Wai.Application
app env = serve monocleAPI $ hoistServer monocleAPI (`runReaderT` env) server

authSettings :: Text -> Text -> Text -> Text -> Auth.AuthSettings
authSettings publicUrl oauthName oauthId oauthSecret =
  Auth.setAuthAppRootStatic publicUrl
    . Auth.setAuthPrefix "auth"
    . Auth.setAuthProviders providers
    . Auth.setAuthSessionAge (3600 * 24 * 7)
    $ Auth.defaultAuthSettings
  where
    emailAllowList = [".*"]
    ghProvider =
      Auth.Provider $
        Auth.mkGithubProvider oauthName oauthId oauthSecret emailAllowList Nothing
    providers = HM.fromList [("github", ghProvider)]

-- | Apply the wai-middleware-auth only on the paths starting with a /a/
--
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- type Middleware = Application -> Application
enforceLoginPath :: Auth.AuthSettings -> Wai.Middleware -> Wai.Middleware
enforceLoginPath as authMiddleware monocleApp = app'
  where
    app' request
      | "/auth/logout" == Wai.rawPathInfo request = doLogout
      | matchAuth (Wai.rawPathInfo request) = authMiddleware monocleApp request
      | otherwise = monocleApp request
    matchAuth path
      | "/a/" `BS.isPrefixOf` path || "/auth" `BS.isPrefixOf` path = True
      | otherwise = False
    doLogout doResp = doResp $ Wai.responseLBS seeOther303 headers ""
      where
        headers =
          [ Auth.getDeleteSessionHeader as,
            ("Location", "/")
          ]

createAuthMiddleware :: IO Wai.Middleware
createAuthMiddleware = do
  envs <- traverse lookupEnv ["PUBLIC_URL", "OAUTH_NAME", "OAUTH_ID", "OAUTH_SECRET"]
  case toText <$> catMaybes envs of
    [publicUrl, oauthName, oauthId, oauthSecret] ->
      let as = authSettings publicUrl oauthName oauthId oauthSecret
       in enforceLoginPath as <$> Auth.mkAuthMiddleware as
    _ -> pure id

run :: MonadIO m => Int -> Text -> FilePath -> m ()
run port elkUrl configFile = do
  tenants' <- Config.loadConfig configFile
  bhEnv' <- I.mkEnv elkUrl

  authMiddleware <- liftIO $ createAuthMiddleware

  liftIO $
    withStdoutLogger $ \aplogger -> do
      let settings = Warp.setPort port $ Warp.setLogger aplogger Warp.defaultSettings
      putTextLn $
        "Serving " <> show (length tenants') <> " tenant(s) on 0.0.0.0:" <> show port <> " with elk: " <> elkUrl
      Warp.runSettings
        settings
        . cors (const $ Just policy)
        . provideOptions monocleAPI
        . authMiddleware
        $ app (Env tenants' bhEnv')
  where
    policy =
      simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
