{-# LANGUAGE AllowAmbiguousTypes #-}

module Effectful.Servant (runWarpServerSettingsContext, hoistEff, handlerToEff) where

import Control.Monad.Except qualified
import Data.Kind (Type)
import Data.Void (Void)
import Effectful
import Effectful.Dispatch.Static qualified
import Effectful.Dispatch.Static.Primitive qualified
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Error.Static qualified as E (throwError)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Prelude

runWarpServerSettingsContext ::
  forall (api :: Type) (context :: [Type]) (es :: [Effect]).
  (HasServer api context, ServerContext context) =>
  Warp.Settings ->
  Context context ->
  Servant.ServerT api (Eff (Error ServerError : es)) ->
  Wai.Middleware ->
  Eff es Void
runWarpServerSettingsContext settings cfg serverEff middleware = do
  Effectful.Dispatch.Static.unsafeEff
    ( \es ->
        Warp.runSettings settings (middleware (hoistEff @api es cfg serverEff))
    )
  error "Warp exited"

hoistEff ::
  forall (api :: Type) (context :: [Type]) (es :: [Effect]).
  (HasServer api context, ServerContext context) =>
  Effectful.Dispatch.Static.Primitive.Env es ->
  Context context ->
  Servant.ServerT api (Eff (Error ServerError : es)) ->
  Wai.Application
hoistEff env ctx = Servant.serveWithContextT (Proxy @api) ctx interpretServer
 where
  interpretServer :: Eff (Error ServerError : es) a -> Servant.Handler a
  interpretServer action = do
    v <- liftIO do
      es' <- Effectful.Dispatch.Static.Primitive.cloneEnv env
      Effectful.Dispatch.Static.unEff (runErrorNoCallStack action) es'
    Control.Monad.Except.liftEither v

-- | Convert a servant handler to eff
handlerToEff :: Error Servant.ServerError Effectful.:> es => Servant.Handler a -> Eff es a
handlerToEff (Servant.Handler (Control.Monad.Except.ExceptT action)) = do
  Effectful.Dispatch.Static.unsafeEff_ action >>= \case
    Left e -> E.throwError e
    Right a -> pure a
