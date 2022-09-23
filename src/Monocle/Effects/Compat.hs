{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides compatible mtl instance.
module Monocle.Effects.Compat where

import Control.Monad.IO.Class (liftIO)
import Effectful (Eff, IOE, (:>))
import Prometheus (MonadMonitor (doIO))

-- | MTL Compat
instance IOE :> es => MonadMonitor (Eff es) where
  doIO = liftIO
