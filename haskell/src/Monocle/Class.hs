-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import qualified Control.Concurrent (modifyMVar, newMVar, threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..))
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

class (MonadRetry m, MonadLog m, MonadTime m, MonadSync m, MonadMonitor m) => MonadGraphQL m where
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

class Monad m => MonadRetry m where
  retry ::
    "policy" ::: RetryPolicyM m ->
    "handler" ::: (RetryStatus -> Handler m Bool) ->
    "action" ::: (Int -> m a) ->
    m a

instance MonadRetry IO where
  retry = retry'

retryLimit :: Int
retryLimit = 7

counterT :: Int -> Int -> Text
counterT count max' = show count <> "/" <> show max'

-- | Use this retry' to implement MonadRetry in IO.
retry' :: (MonadMask m, MonadIO m) => RetryPolicyM m -> (RetryStatus -> Handler m Bool) -> (Int -> m a) -> m a
retry' policy handler baseAction =
  Retry.recovering
    policy
    [handler]
    action
  where
    action (RetryStatus num _ _) = baseAction num

-- | Retry HTTP network action, doubling backoff each time
httpRetry :: (MonadLog m, MonadMonitor m, MonadRetry m) => (Text, Text, Text) -> m a -> m a
httpRetry label baseAction = retry policy httpHandler (const action)
  where
    backoff = 500000 -- 500ms
    policy = Retry.exponentialBackoff backoff <> Retry.limitRetries retryLimit
    action = do
      res <- baseAction
      incrementCounter httpRequestCounter label
      pure res
    httpHandler (RetryStatus num _ _) = Handler $ \case
      HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        mLog . Log Unspecified . LogNetworkFailure $
          counterT num retryLimit
            <> " "
            <> loc
            <> " failed: "
            <> show ctx
        incrementCounter httpFailureCounter label
        pure True
      InvalidUrlException _ _ -> pure False

-- | A retry helper with a constant policy. This helper is in charge of low level logging
-- and TODO: incrementCounter for graphql request and errors
constantRetry :: (MonadRetry m, MonadLog m) => Text -> Handler m Bool -> (Int -> m a) -> m a
constantRetry msg handler baseAction = retry policy (const handler) action
  where
    delay = 1_100_000 -- 1.1 seconds
    policy = Retry.constantDelay delay <> Retry.limitRetries retryLimit
    action num = do
      when (num > 0) $
        mLog . Log Macroscope . LogRaw $ counterT num retryLimit <> " failed: " <> msg
      baseAction num
