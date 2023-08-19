-- | This module contains the new monocle app
module Monocle.Butler (
  -- * The websocket API
  ButlerWSAPI,
  butlerWsApp,

  -- * UI Apps
  dashboardApp,

  -- * Adapter for running ProcessIO inside effectful
  runButlerEffect,
  runButlerProcess,
  withButlerDisplay,
) where

import Butler
import Butler.App
import Butler.Core
import Butler.Display
import Butler.Display.Session
import Butler.Display.WebSocket
import Network.WebSockets qualified as WS
import Prelude

import Monocle.Api.Jwt (AuthenticatedUser (..), OIDCEnv)
import Monocle.Backend.Queries qualified as Q
import Monocle.Effects qualified as E
import Monocle.Env qualified as M
import Monocle.Logging qualified as E (runLoggerEffect)
import Monocle.Prelude (runEff)

import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket (SockAddr)
import Network.Wai qualified as Wai
import Servant qualified

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, withEffToIO, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unEff, unsafeEff, unsafeEff_)
import Effectful.Internal.Env (cloneEnv)

data ButlerEffect :: Effect
type instance DispatchOf ButlerEffect = 'Static 'WithSideEffects
newtype instance StaticRep ButlerEffect = ButlerEffect ProcessEnv

runButlerEffect :: IOE :> es => (ProcessEnv -> Eff (ButlerEffect : es) ()) -> Eff es ()
runButlerEffect action =
  Effectful.Dispatch.Static.unsafeEff \es -> do
    exit <- withButlerOS do
      env <- ask
      liftIO do
        es' <- cloneEnv es
        unEff (evalStaticRep (ButlerEffect env) (action env)) es'
    print exit

runButlerProcess :: ButlerEffect :> es => ProcessIO a -> Eff es a
runButlerProcess action = do
  ButlerEffect processEnv <- getStaticRep
  unsafeEff_ $ runProcessIOEnv processEnv action

_mkMiddleware :: Wai.Application -> Wai.Middleware
_mkMiddleware app baseApp req resp = baseApp req handleAppResp
 where
  handleAppResp appResp = case HTTP.statusCode (Wai.responseStatus appResp) of
    404 -> app req resp
    _ -> resp appResp

withButlerDisplay :: (IOE :> es, ButlerEffect :> es) => (Display -> Eff es a) -> Eff es a
withButlerDisplay cb = do
  ButlerEffect processEnv <- getStaticRep
  withEffToIO $ \runInIO -> do
    runProcessIOEnv processEnv $ withSessions "sessions" \sessions -> do
      display <- atomically (newDisplay sessions)
      liftIO $ runInIO (cb display)

type ButlerWSAPI = WebSocketAPI AuthenticatedUser

butlerWsApp :: Maybe OIDCEnv -> ProcessEnv -> Display -> [App] -> Servant.ServerT ButlerWSAPI Servant.Handler
butlerWsApp mAuth processEnv display apps = websocketServer processEnv adaptMonocleSession onConnect
 where
  adaptMonocleSession :: Maybe AuthenticatedUser -> ProcessIO (Maybe Session)
  adaptMonocleSession = \case
    Nothing
      | isNothing mAuth -> Just <$> newSession display.sessions Nothing "guest"
      | otherwise -> pure Nothing
    Just aUser -> do
      let user = UserName aUser.aDefaultMuid
          provider = externalProvider "monocle" user
      atomically (lookupSessionByProvider display.sessions provider) >>= \case
        Just session -> pure (Just session)
        Nothing -> Just <$> newSession display.sessions (Just provider) user

  onConnect :: SockAddr -> Workspace -> ChannelName -> Session -> WS.Connection -> ProcessIO ()
  onConnect = connectRoute display "embeded-server-name" onClient

  onClient :: OnClient
  onClient session workspace = do
    logInfo "New client!" ["sess" .= session, "workspace" .= workspace]
    shared <- startApps apps display
    pure (processEnv, staticClientHandler shared)

-- Here is a demo app that only display the current change count.
-- Given a elasticEnv, we can unwrap the effectful Eff context into butler's ProcessIO
dashboardApp :: E.ElasticEnv -> App
dashboardApp elasticEnv = defaultApp "dashboard" startDashboard
 where
  runEffects = liftIO . runEff . E.runLoggerEffect . E.runElasticEffect elasticEnv . E.runMonoQuery queryEnv

  startDashboard ctx = do
    state <- newTVarIO 0
    let getChanges = runEffects do
          -- Here is the demo of using the monocle backend:
          count <- Q.countDocs
          atomically do writeTVar state count

    -- make a query
    getChanges

    let mountUI = with div_ [wid_ ctx.wid "w"] do
          "Change count: "
          count <- lift do readTVar state
          toHtml (showT count)

    forever do
      atomically (readPipe ctx.pipe) >>= \case
        ae@AppDisplay {} -> sendHtmlOnConnect mountUI ae
        _ -> pure ()

  -- TODO: make this configurable by the user.
  queryEnv :: E.MonoQueryEnv
  queryEnv =
    E.MonoQueryEnv
      { queryTarget = M.QueryWorkspace (M.mkConfig "openstack")
      , searchQuery = undefined
      }
