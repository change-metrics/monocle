{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A shared library between lentilles and macroscope
module Lentille
  ( -- * The lentille context
    LentilleM,
    LentilleStream,
    runLentilleM,
    stopLentille,

    -- * Lentille Errors
    LentilleError (..),
  )
where

import Control.Monad.Except (MonadError, throwError)
import Monocle.Api.Client.Worker (MonadLog (..), MonadTime (..), getCurrentTime, logEvent)
import Monocle.Prelude (MonadThrow)
import Relude
import Streaming (Of, Stream)

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ExceptT LentilleError IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow)
  deriving newtype (MonadError LentilleError)

instance MonadTime LentilleM where
  getTime = liftIO getCurrentTime

instance MonadLog LentilleM where
  log' = logEvent
  log = void . log'

data LentilleError
  = DecodeError [Text]
  deriving (Show)

type LentilleStream a = Stream (Of a) LentilleM ()

runLentilleM :: MonadIO m => LentilleM a -> m (Either LentilleError a)
runLentilleM = liftIO . runExceptT . unLentille

stopLentille :: LentilleError -> LentilleStream a
stopLentille = throwError
