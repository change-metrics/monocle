-- | Monocle log events
-- Note: [Monocle Structured Logging]
--
-- Structure comes from the 'Data.Aeson.Series'. They can be created using
--    key .= value
-- And combined using mappend:
--    key1 .= value1 <> key2 .= value2
module Monocle.Logging (
  -- * Logging effect
  HasLogger (..),
  LogLevel,
  logWarn,
  logWarn_,
  logInfo,
  logInfo_,
  logDebug,
  logDebug_,

  -- * The logger object
  Logger,
  addCtx,
  withLogger,

  -- * Standalone logging implementation (for use outside of Monocle.Env or LentilleM)
  LoggerT,
  runLogger,
  runLogger',
) where

import Data.Aeson (Series, pairs)
import Data.Aeson.Encoding (encodingToLazyByteString)
import GHC.Stack
import Prometheus qualified
import System.Log.FastLogger qualified as FastLogger

import Monocle.Prelude

-- | The logger effect definition, to be implemented by custom monad such as LentilleM
class Monad m => HasLogger m where
  getLogger :: m Logger
  withContext :: Series -> m () -> m ()
  logIO :: IO () -> m ()

data LogLevel = LogWarning | LogInfo | LogDebug

instance From LogLevel ByteString where
  from = \case
    LogWarning -> "WARNING "
    LogInfo -> "INFO    "
    LogDebug -> "DEBUG   "

-- | doLog outputs a oneliner text message
doLog :: HasLogger m => LogLevel -> ByteString -> Text -> [Series] -> m ()
doLog lvl loc msg attrs = do
  Logger ctx logger <- getLogger
  let body :: ByteString
      body = case from . encodingToLazyByteString . pairs . mappend ctx . mconcat $ attrs of
        "{}" -> mempty
        x -> " " <> x
  logIO $ logger (\time -> FastLogger.toLogStr $ time <> msgText <> body <> "\n")
 where
  msgText :: ByteString
  msgText = from lvl <> loc <> ": " <> encodeUtf8 msg

-- | Get the `Module.Name:LINE` from the log* caller, jumping over the log* stack
getLocName :: HasCallStack => ByteString
getLocName = case getCallStack callStack of
  (_logStack : (_, srcLoc) : _) -> from (srcLocModule srcLoc) <> ":" <> show (srcLocStartLine srcLoc)
  _ -> "N/C"

-- | Produce info log with attributes, for example:
--
-- logInfo "Starting" ["ip" .= addr, "port" .= 42]
logInfo :: (HasCallStack, HasLogger m) => Text -> [Series] -> m ()
logInfo = doLog LogInfo getLocName

-- | Produce info log without attributes.
logInfo_ :: (HasCallStack, HasLogger m) => Text -> m ()
logInfo_ msg = doLog LogInfo getLocName msg []

-- | Produce messages that need attention.
logWarn :: (HasCallStack, HasLogger m) => Text -> [Series] -> m ()
logWarn = doLog LogWarning getLocName

logWarn_ :: (HasCallStack, HasLogger m) => Text -> m ()
logWarn_ msg = doLog LogWarning getLocName msg []

-- | Produce trace logs.
logDebug :: (HasCallStack, HasLogger m) => Text -> [Series] -> m ()
logDebug = doLog LogDebug getLocName

logDebug_ :: (HasCallStack, HasLogger m) => Text -> m ()
logDebug_ msg = doLog LogDebug getLocName msg []

-- | The logger representation, it is opaque for users.
data Logger = Logger
  { _ctx :: Series
  , _logger :: FastLogger.TimedFastLogger
  }

addCtx :: Series -> Logger -> Logger
addCtx ctx (Logger prev logger) = Logger (ctx <> prev) logger

-- | withLogger create the logger
withLogger :: (Logger -> IO a) -> IO a
withLogger cb = do
  tc <- liftIO $ FastLogger.newTimeCache "%F %T "
  FastLogger.withTimedFastLogger tc logger (cb . Logger mempty)
 where
  logger = FastLogger.LogStderr 1024

-- | A standalone HasLogger implementation
newtype LoggerT a = LoggerT (ReaderT Logger IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Logger, MonadMask, MonadCatch, MonadThrow, MonadUnliftIO, Prometheus.MonadMonitor)

instance HasLogger LoggerT where
  getLogger = ask
  withContext series = local (addCtx series)
  logIO = liftIO

runLogger :: MonadIO m => Logger -> LoggerT a -> m a
runLogger logger (LoggerT action) = liftIO $ runReaderT action logger

runLogger' :: MonadIO m => LoggerT a -> m a
runLogger' (LoggerT action) = liftIO $ withLogger (runReaderT action)
