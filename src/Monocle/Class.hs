-- disable redundant constraint warning for fake effect
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import Control.Concurrent (threadDelay)
import Data.Time.Clock qualified (getCurrentTime)
import Monocle.Prelude

import Effectful (Dispatch (Static), DispatchOf)
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep)

import Effectful.Prometheus

runMonitoring :: IOE :> es => Eff (LoggerEffect : PrometheusEffect : TimeEffect : Retry : es) a -> Eff es a
runMonitoring = runRetry . runTime . runPrometheus . runLoggerEffect

-------------------------------------------------------------------------------
-- A time system

data TimeEffect :: Effect
type instance DispatchOf TimeEffect = 'Static 'WithSideEffects
data instance StaticRep TimeEffect = TimeEffect
runTime :: IOE :> es => Eff (TimeEffect : es) a -> Eff es a
runTime = evalStaticRep TimeEffect

mGetCurrentTime :: TimeEffect :> es => Eff es UTCTime
mGetCurrentTime = unsafeEff_ Data.Time.Clock.getCurrentTime

mThreadDelay :: TimeEffect :> es => Int -> Eff es ()
mThreadDelay = unsafeEff_ . threadDelay

holdOnUntil :: TimeEffect :> es => UTCTime -> Eff es ()
holdOnUntil resetTime = do
  currentTime <- mGetCurrentTime
  let delaySec = diffTimeSec resetTime currentTime + 1
  mThreadDelay $ delaySec * 1_000_000
