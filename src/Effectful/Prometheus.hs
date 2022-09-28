-- disable redundant constraint warning for fake effect
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Effectful.Prometheus (
  PrometheusEffect,
  runPrometheus,
  promIncrCounter,
) where

import Data.Text (Text)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, unsafeEff_)
import Prometheus qualified

data PrometheusEffect :: Effect
type instance DispatchOf PrometheusEffect = 'Static 'WithSideEffects
data instance StaticRep PrometheusEffect = PrometheusEffect

runPrometheus :: IOE :> es => Eff (PrometheusEffect : es) a -> Eff es a
runPrometheus = evalStaticRep PrometheusEffect

-- Text tuple, e.g. ("module", "url")
type PromLabels a = Prometheus.Vector (Text, Text) a

promIncrCounter :: PrometheusEffect :> es => PromLabels Prometheus.Counter -> (Text, Text) -> Eff es ()
promIncrCounter x l = unsafeEff_ (Prometheus.withLabel x l Prometheus.incCounter)
