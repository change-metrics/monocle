-- | This module contains the new monocle app
module Monocle.Butler where

import Butler
import Prelude

import Monocle.Backend.Queries qualified as Q
import Monocle.Effects qualified as E
import Monocle.Env qualified as M
import Monocle.Logging qualified as E (runLoggerEffect)
import Monocle.Prelude (runEff)

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
