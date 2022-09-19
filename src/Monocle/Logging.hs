{-# LANGUAGE DeriveAnyClass #-}

-- | Monocle log events
module Monocle.Logging where

import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.OneLine qualified
import GHC.Stack
import Prometheus qualified
import System.Log.FastLogger qualified as FastLogger

import Monocle.Entity
import Monocle.Prelude

data LogCrawlerContext = LogCrawlerContext
  { lccIndex :: Text
  , lccName :: Text
  , lccEntity :: Maybe Entity
  }
  deriving (Generic, ToJSON)

noContext :: LogCrawlerContext
noContext = LogCrawlerContext "<direct>" "CLI" Nothing

-----------------------------------------------------------
-- Logging facilities

type Logger = FastLogger.TimedFastLogger

class Monad m => HasLogger m where
  getLogger :: m Logger
  logIO :: IO () -> m ()

newtype LoggerT a = LoggerT (ReaderT Logger IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Logger, MonadMask, MonadCatch, MonadThrow, MonadUnliftIO, Prometheus.MonadMonitor)

runLogger :: MonadIO m => Logger -> LoggerT a -> m a
runLogger logger (LoggerT action) = liftIO $ runReaderT action logger

runLogger' :: MonadIO m => LoggerT a -> m a
runLogger' (LoggerT action) = liftIO $ withLogger (runReaderT action)

instance HasLogger LoggerT where
  getLogger = ask
  logIO = liftIO

data LogLevel = LogWarning | LogInfo | LogDebug

instance From LogLevel Text where
  from = \case
    LogWarning -> "WARNING "
    LogInfo -> "INFO    "
    LogDebug -> "DEBUG   "

goLog :: HasLogger m => LogLevel -> Text -> Text -> [Pair] -> m ()
goLog lvl loc msg attrs = do
  logger <- getLogger
  logIO $ logger (\time -> FastLogger.toLogStr $ time <> from msgText <> "\n")
 where
  msgText :: Text
  msgText =
    from lvl <> loc <> ": " <> msg <> case attrs of
      [] -> mempty
      _ -> " " <> Data.Aeson.OneLine.renderObject (KM.fromList attrs)

getLocName :: HasCallStack => Text
getLocName = case getCallStack callStack of
  (_ : (_, srcLoc) : _) -> from $ srcLocModule srcLoc <> ":" <> show (srcLocStartLine srcLoc)
  _ -> "N/C"

logInfo :: (HasCallStack, HasLogger m) => Text -> [Pair] -> m ()
logInfo = goLog LogInfo getLocName

logInfo_ :: (HasCallStack, HasLogger m) => Text -> m ()
logInfo_ msg = goLog LogInfo getLocName msg []

logWarn :: (HasCallStack, HasLogger m) => Text -> [Pair] -> m ()
logWarn = goLog LogWarning getLocName

logWarn_ :: (HasCallStack, HasLogger m) => Text -> m ()
logWarn_ msg = goLog LogWarning getLocName msg []

logDebug :: (HasCallStack, HasLogger m) => Text -> [Pair] -> m ()
logDebug = goLog LogDebug getLocName

logDebug_ :: (HasCallStack, HasLogger m) => Text -> m ()
logDebug_ msg = goLog LogDebug getLocName msg []

-- | withLogger create the logger
withLogger :: (Logger -> IO a) -> IO a
withLogger cb = do
  tc <- liftIO $ FastLogger.newTimeCache "%F %T "
  FastLogger.withTimedFastLogger tc logger cb
 where
  logger = FastLogger.LogStderr 1024
