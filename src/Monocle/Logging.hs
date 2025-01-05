-- | Monocle log events
-- Note: [Monocle Structured Logging]
--
-- Structure comes from the 'Data.Aeson.Series'. They can be created using
--    key .= value
-- And combined using mappend:
--    key1 .= value1 <> key2 .= value2
module Monocle.Logging (
  -- * Logger API
  logWarn,
  logWarn_,
  logInfo,
  logInfo_,
  logDebug,
  logDebug_,
  withContext,

  -- * Logger effect
  LoggerEffect,
  runLoggerEffect,

  -- * Logger IO
  Logger (..),
  withLogger,
) where

import Control.Monad.IO.Class (liftIO)
import GHC.Stack
import Prelude

import Data.Aeson (Series, pairs)
import Data.Aeson.Encoding (encodingToLazyByteString)
import System.Log.FastLogger qualified as FastLogger

import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, UnliftStrategy (SeqUnlift), withEffToIO, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, localStaticRep, unsafeEff_)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Witch (from)

data LogLevel = LogWarning | LogInfo | LogDebug

logLevelBS :: LogLevel -> ByteString
logLevelBS = \case
  LogWarning -> "WARNING "
  LogInfo -> "INFO    "
  LogDebug -> "DEBUG   "

-- | doLog outputs a oneliner text message
doLog :: LoggerEffect :> es => LogLevel -> ByteString -> Text -> [Series] -> Eff es ()
doLog lvl loc msg attrs = do
  LoggerEffect (Logger ctx logger) <- getStaticRep
  let body :: ByteString
      body = case from . encodingToLazyByteString . pairs . mappend ctx . mconcat $ attrs of
        "{}" -> mempty
        x -> " " <> x
  -- `unsafeEff_` is equivalent to `liftIO`
  unsafeEff_ $ logger (\time -> FastLogger.toLogStr $ time <> msgText <> body <> "\n")
 where
  msgText :: ByteString
  msgText = logLevelBS lvl <> loc <> ": " <> encodeUtf8 msg

-- | Get the `Module.Name:LINE` from the log* caller, jumping over the log* stack
getLocName :: HasCallStack => ByteString
getLocName = case getCallStack callStack of
  (_logStack : (_, srcLoc) : _) ->
    let modName = from $ srcLocModule srcLoc
        line = from $ show $ srcLocStartLine srcLoc
     in encodeUtf8 $ modName <> ":" <> line
  _ -> "N/C"

-- | Produce info log with attributes, for example:
--
-- logInfo "Starting" ["ip" .= addr, "port" .= 42]
logInfo :: (HasCallStack, LoggerEffect :> es) => Text -> [Series] -> Eff es ()
logInfo = doLog LogInfo getLocName

-- | Produce info log without attributes.
logInfo_ :: (HasCallStack, LoggerEffect :> es) => Text -> Eff es ()
logInfo_ msg = doLog LogInfo getLocName msg []

-- | Produce messages that need attention.
logWarn :: (HasCallStack, LoggerEffect :> es) => Text -> [Series] -> Eff es ()
logWarn = doLog LogWarning getLocName

logWarn_ :: (HasCallStack, LoggerEffect :> es) => Text -> Eff es ()
logWarn_ msg = doLog LogWarning getLocName msg []

-- | Produce trace logs.
logDebug :: (HasCallStack, LoggerEffect :> es) => Text -> [Series] -> Eff es ()
logDebug = doLog LogDebug getLocName

logDebug_ :: (HasCallStack, LoggerEffect :> es) => Text -> Eff es ()
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

-- | The effect definition
type LoggerEnv = Logger

data LoggerEffect :: Effect
type instance DispatchOf LoggerEffect = 'Static 'WithSideEffects
newtype instance StaticRep LoggerEffect = LoggerEffect LoggerEnv

runLoggerEffect :: IOE :> es => Eff (LoggerEffect : es) a -> Eff es a
runLoggerEffect action =
  -- `withEffToIO` and `unInIO` enables calling IO function like: `(Logger -> IO a) -> IO a`.
  withEffToIO SeqUnlift $ \runInIO ->
    withLogger \logger ->
      runInIO $ evalStaticRep (LoggerEffect logger) action

withContext :: LoggerEffect :> es => Series -> Eff es a -> Eff es a
withContext ctx = localStaticRep $ \(LoggerEffect logger) -> LoggerEffect (addCtx ctx logger)
