{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides compatible mtl instance.
module Monocle.Effects.Compat where

import Effectful (IOE, Eff, (:>))
import Prometheus (MonadMonitor(doIO))
import Control.Monad.IO.Class (liftIO)

-- | MTL Compat
instance IOE :> es => MonadMonitor (Eff es) where
  doIO = liftIO
