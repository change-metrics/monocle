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

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, throwError)
import Monocle.Client.Worker (MonadLog (..), MonadTime (..), getCurrentTime, logEvent)
import Monocle.Prelude (MonadMask, MonadThrow)
import Relude
import Streaming (Of, Stream)

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ExceptT LentilleError IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadError LentilleError)

instance MonadTime LentilleM where
  getTime = getCurrentTime

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
