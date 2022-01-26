-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import qualified Control.Concurrent (modifyMVar, newMVar, threadDelay)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import qualified Data.Time.Clock (getCurrentTime)
import Monocle.Client (MonocleClient, mkManager)
import Monocle.Client.Api (crawlerAddDoc, crawlerCommit, crawlerCommitInfo)
import Monocle.Crawler (AddDocRequest, AddDocResponse, CommitInfoRequest, CommitInfoResponse, CommitRequest, CommitResponse)
import Monocle.Logging
import Monocle.Prelude
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as HTTP

-------------------------------------------------------------------------------
-- A time system

class Monad m => MonadTime m where
  mGetCurrentTime :: m UTCTime
  mThreadDelay :: Int -> m ()

instance MonadTime IO where
  mGetCurrentTime = Data.Time.Clock.getCurrentTime
  mThreadDelay = Control.Concurrent.threadDelay

holdOnUntil :: (MonadTime m) => UTCTime -> m ()
holdOnUntil resetTime = do
  currentTime <- mGetCurrentTime
  let delaySec = diffTimeSec resetTime currentTime + 1
  mThreadDelay $ delaySec * 1_000_000

-------------------------------------------------------------------------------
-- A concurrent system handled via Control.Concurrent.MVar

class Monad m => MonadSync m where
  mNewMVar :: a -> m (MVar a)
  mModifyMVar :: MVar a -> (a -> m (a, b)) -> m b

instance MonadSync IO where
  mNewMVar = Control.Concurrent.newMVar
  mModifyMVar = Control.Concurrent.modifyMVar

-------------------------------------------------------------------------------
-- A log system
data LogAuthor = Macroscope | Unspecified

instance From LogAuthor Text where
  from = \case
    Macroscope -> "Macroscope"
    Unspecified -> "Unknown"

data Log = Log {author :: LogAuthor, event :: LogEvent}

instance From Log Text where
  from Log {..} = from author <> ": " <> from event

class (Monad m, MonadTime m) => MonadLog m where
  mLog :: Log -> m ()

instance MonadLog IO where
  mLog = logText . from

-------------------------------------------------------------------------------
-- A GraphQL client system

class (MonadRetry m, MonadLog m, MonadTime m, MonadSync m) => MonadGraphQL m where
  httpRequest :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LByteString)
  newManager :: m HTTP.Manager

instance MonadGraphQL IO where
  httpRequest = HTTP.httpLbs
  newManager = mkManager

-------------------------------------------------------------------------------
-- The Monocle Crawler system

class Monad m => MonadCrawler m where
  mReadIORef :: IORef a -> m a
  mCrawlerAddDoc :: MonocleClient -> AddDocRequest -> m AddDocResponse
  mCrawlerCommit :: MonocleClient -> CommitRequest -> m CommitResponse
  mCrawlerCommitInfo :: MonocleClient -> CommitInfoRequest -> m CommitInfoResponse

instance MonadCrawler IO where
  mReadIORef = readIORef
  mCrawlerAddDoc = crawlerAddDoc
  mCrawlerCommit = crawlerCommit
  mCrawlerCommitInfo = crawlerCommitInfo

-------------------------------------------------------------------------------
-- A network retry system

-- TODO: remove retry in favor of genericRetry
class Monad m => MonadRetry m where
  retry :: (Text, Text, Text) -> m a -> m a
  genericRetry :: LogAuthor -> Text -> (RetryStatus -> a -> m Bool) -> Int -> (Int -> m a) -> m a

instance MonadRetry IO where
  retry = retry'
  genericRetry = genericRetry'

counterT :: Int -> Int -> Text
counterT count max' = show count <> "/" <> show max'

-- | Retry HTTP network action, doubling backoff each time
-- Use this retry' to implement MonadRetry in IO.
retry' ::
  (MonadMask m, MonadLog m, MonadIO m, MonadMonitor m) =>
  (Text, Text, Text) ->
  m a ->
  m a
retry' label baseAction =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries limit)
    [handler]
    (const action)
  where
    limit = 7
    action = do
      res <- baseAction
      incrementCounter httpRequestCounter label
      pure res
    backoff = 500000 -- 500ms
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        mLog . Log Unspecified . LogNetworkFailure $
          counterT num limit
            <> " "
            <> loc
            <> " failed: "
            <> show ctx
        incrementCounter httpFailureCounter label
        pure True
      InvalidUrlException _ _ -> pure False

-- | Retry IO action, with a constant configuration delay.
-- | Action to retry get the current retry attempt value to optionaly
-- | adapt its behavior.
-- Use this genricRetry' to implement MonadRetry in IO.
genericRetry' ::
  (MonadMask m, MonadIO m, MonadLog m) =>
  -- The Log emitter author
  LogAuthor ->
  -- A log message to display at each attempt
  Text ->
  -- The check function that to determine whether or not to retry
  (RetryStatus -> a -> m Bool) ->
  -- The constant delay to wait for between retries (in millisecond)
  Int ->
  -- The IO action to retry if needed
  (Int -> m a) ->
  m a
genericRetry' author msg checker delay baseAction =
  Retry.retrying
    (Retry.constantDelay delay <> Retry.limitRetries limit)
    checker
    action
  where
    limit = 7
    action (RetryStatus num _ _) = do
      when (num > 0) $
        mLog . Log author . LogRaw $ counterT num limit <> " failed: " <> msg
      baseAction num
