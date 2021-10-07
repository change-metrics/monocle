-- | A shared library between lentilles and macroscope
module Lentille
  ( -- * The lentille context
    LentilleM (..),
    LentilleStream,
    runLentilleM,
    stopLentille,

    -- * Lentille Errors
    LentilleError (..),

    -- * Log context
    MonadTime (..),
    MonadLog (..),
    LogEvent (..),
    logEvent,

    -- * Retry context
    MonadRetry (..),
    retry',
  )
where

import Control.Monad.Except (MonadError, throwError)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Monocle.Crawler as CrawlerPB
import Monocle.Prelude
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as HTTP

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ExceptT LentilleError IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadError LentilleError)

instance MonadTime LentilleM where
  getTime = getCurrentTime

instance MonadRetry LentilleM where
  retry = retry'

instance MonadLog LentilleM where
  log' = logEvent

data LentilleError
  = DecodeError [Text]
  deriving (Show)

type LentilleStream a = Stream (Of a) LentilleM ()

runLentilleM :: MonadIO m => LentilleM a -> m (Either LentilleError a)
runLentilleM = liftIO . runExceptT . unLentille

stopLentille :: LentilleError -> LentilleStream a
stopLentille = throwError

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------
data LogEvent
  = LogStarting
  | LogStartingEntity CrawlerPB.CommitInfoRequest_EntityType
  | LogEnded
  | LogFailed
  | LogNetworkFailure Text
  | LogOldestEntity CrawlerPB.CommitInfoResponse_OldestEntity
  | LogGetBugs UTCTime Int Int
  | LogPostData Int

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance MonadTime IO where
  getTime = getCurrentTime

logEvent :: (MonadTime m, MonadIO m) => LogEvent -> m UTCTime
logEvent ev = do
  now <- getTime
  putTextLn $ "[" <> showTime now <> "]: " <> evStr
  pure now
  where
    showTime now = toText . take 23 $ formatTime defaultTimeLocale "%F %T.%q" now
    evStr = case ev of
      LogStarting -> "Starting updates"
      LogStartingEntity e -> "Starting updates for " <> show e
      LogEnded -> "Update completed"
      LogFailed -> "Commit failed"
      LogNetworkFailure msg -> "Network error: " <> msg
      LogGetBugs ts offset limit ->
        "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
      LogPostData count -> "Posting tracker data " <> show count
      LogOldestEntity oe -> "Got entity " <> show oe

class Monad m => MonadLog m where
  log' :: LogEvent -> m UTCTime
  log :: LogEvent -> m ()
  log = void . log'

instance MonadLog IO where
  log' = logEvent

-------------------------------------------------------------------------------
-- Network Retry system
-------------------------------------------------------------------------------
class MonadRetry m where
  retry :: m a -> m a

instance MonadRetry IO where
  retry = retry'

-- | Use this retry'prime to implement MonadRetry in IO.
-- Retry 5 times network action, doubling backoff each time
retry' :: (MonadMask m, MonadLog m, MonadIO m) => m a -> m a
retry' action =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 6)
    [handler]
    (const action)
  where
    backoff = 500000 -- 500ms
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        log . LogNetworkFailure $ show num <> "/6 " <> loc <> " failed: " <> show ctx
        pure True
      InvalidUrlException _ _ -> pure False
