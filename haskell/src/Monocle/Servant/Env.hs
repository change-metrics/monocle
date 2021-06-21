{-# LANGUAGE FlexibleInstances #-}

-- | The api configuration environment
module Monocle.Servant.Env where

import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Servant (Handler)

data Env = Env
  { tenants :: [Config.Index],
    bhEnv :: BH.BHEnv
  }

instance BH.MonadBH (ReaderT Env Handler) where
  getBHEnv = asks bhEnv

type AppM = ReaderT Env Handler
