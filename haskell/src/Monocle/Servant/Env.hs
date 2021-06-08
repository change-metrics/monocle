-- | The api configuration environment
module Monocle.Servant.Env where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Servant (Handler)

data Env = Env
  { tenants :: [Config.Tenant],
    bhEnv :: BH.BHEnv
  }

type AppM = ReaderT Env Handler
