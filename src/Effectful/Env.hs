{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Effectful.Env (
  EnvEffect,
  runEnv,
  envGet,
) where

import Data.ByteString (ByteString)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, unsafeEff_)
import System.Posix.Env.ByteString (getEnv)
import Prelude (Maybe)

data EnvEffect :: Effect
type instance DispatchOf EnvEffect = 'Static 'WithSideEffects
data instance StaticRep EnvEffect = EnvEffect

runEnv :: IOE :> es => Eff (EnvEffect : es) a -> Eff es a
runEnv = evalStaticRep EnvEffect

envGet :: EnvEffect :> es => ByteString -> Eff es (Maybe ByteString)
envGet e = unsafeEff_ (getEnv e)
