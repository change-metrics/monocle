{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The api configuration environment
module Monocle.Servant.Env (Env (..), AppM (..), TenantM, runTenantM, runTenantM', tenantIndexName, getIndexName, getIndexConfig) where

import Control.Monad.Catch (MonadThrow)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Relude
import Servant (Handler)

-- | 'Env' is the global environment
data Env = Env
  { tenants :: [Config.Index],
    bhEnv :: BH.BHEnv
  }

-- | 'AppM' is the main context, it just adds Env to the servant Handler using Reader
newtype AppM a = AppM {unApp :: ReaderT Env Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadReader Env)

-- | 'TenantEnv' is the request environment, after validation
data TenantEnv = TenantEnv
  { tenant :: Config.Index,
    bhEnv' :: BH.BHEnv
  }

-- | 'TenantM' is the request context, it contains the TenantEnv
newtype TenantM a = TenantM {unTenant :: ReaderT TenantEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)
  deriving newtype (MonadReader TenantEnv)

tenantIndexName :: Config.Index -> BH.IndexName
tenantIndexName Config.Index {..} = BH.IndexName $ "monocle.changes.1." <> index

-- | 'runTenantM' is the only way to run an 'TenantM' computation
runTenantM :: Config.Index -> TenantM a -> AppM a
runTenantM config (TenantM im) = do
  bhEnv <- BH.getBHEnv
  liftIO $ runReaderT im (TenantEnv config bhEnv)

-- | 'runTenantM'' is used in test, without the servant Handler
runTenantM' :: forall a. BH.BHEnv -> Config.Index -> TenantM a -> IO a
runTenantM' bhEnv config tenantM =
  runReaderT (unTenant tenantM) (TenantEnv config bhEnv)

-- | We can derive a MonadBH from AppM, we just needs to tell 'getBHEnv' where is BHEnv
instance BH.MonadBH AppM where
  getBHEnv = asks bhEnv

-- | We can also derive a MonadBH from TenantM, we just needs to lift to the parent Reader
instance BH.MonadBH TenantM where
  getBHEnv = TenantM (asks bhEnv')

getIndexName :: TenantM BH.IndexName
getIndexName = tenantIndexName <$> asks tenant

getIndexConfig :: TenantM Config.Index
getIndexConfig = asks tenant

instance MonadFail AppM where
  fail = error . toText
