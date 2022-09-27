-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import Control.Concurrent qualified (modifyMVar, newMVar, threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..))
import Control.Retry qualified as Retry
import Data.Time.Clock qualified (getCurrentTime)
import Monocle.Client (MonocleClient, mkManager)
import Monocle.Client.Api (crawlerAddDoc, crawlerCommit, crawlerCommitInfo)
import Monocle.Prelude
import Monocle.Protob.Crawler (
  AddDocRequest,
  AddDocResponse,
  CommitInfoRequest,
  CommitInfoResponse,
  CommitRequest,
  CommitResponse,
 )
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Client qualified as HTTP

import Effectful (Dispatch (Static), DispatchOf)
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep)

import Effectful.Prometheus

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
-- A GraphQL client system

class (MonadRetry m, MonadTime m, MonadSync m, MonadMonitor m) => MonadGraphQL m where
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

data RetryEffect :: Effect

type instance DispatchOf RetryEffect = 'Static 'WithSideEffects
data instance StaticRep RetryEffect = RetryEffect

runRetry :: IOE :> es => Eff (RetryEffect : es) a -> Eff es a
runRetry = evalStaticRep RetryEffect

retry ::
  forall es a.
  (RetryEffect :> es) =>
  "policy" ::: RetryPolicyM (Eff es) ->
  "handler" ::: (RetryStatus -> Handler (Eff es) Bool) ->
  "action" ::: (Int -> Eff es a) ->
  Eff es a
retry (Retry.RetryPolicyM policy) handler action =
  unsafeEff $ \env ->
    let actionIO :: RetryStatus -> IO a
        actionIO (RetryStatus num _ _) = unEff (action num) env
        policyIO :: RetryPolicyM IO
        policyIO = Retry.RetryPolicyM $ \s -> unEff (policy s) env
        convertHandler :: Handler (Eff es) Bool -> Handler IO Bool
        convertHandler (Handler handlerEff) =
          Handler $ \e -> unEff (handlerEff e) env
        handlerIO :: RetryStatus -> Handler IO Bool
        handlerIO s = convertHandler (handler s)
     in Retry.recovering policyIO [handlerIO] actionIO

retryLimit :: Int
retryLimit = 7

counterT :: Int -> Int -> Text
counterT count max' = show count <> "/" <> show max'

-- | Retry HTTP network action, doubling backoff each time
httpRetry :: (HasCallStack, [PrometheusEffect, RetryEffect] :>> es) => Text -> Eff es a -> Eff es a
httpRetry urlLabel baseAction = retry policy httpHandler (const action)
 where
  modName = case getCallStack callStack of
    ((_, srcLoc) : _) -> from (srcLocModule srcLoc)
    _ -> "N/C"
  label = (modName, urlLabel)

  backoff = 500000 -- 500ms
  policy = Retry.exponentialBackoff backoff <> Retry.limitRetries retryLimit
  action = do
    res <- baseAction
    promIncrCounter httpRequestCounter label
    pure res
  httpHandler (RetryStatus num _ _) = Handler $ \case
    HttpExceptionRequest req ctx -> do
      let url = decodeUtf8 @Text $ HTTP.host req <> ":" <> show (HTTP.port req) <> HTTP.path req
          arg = decodeUtf8 $ HTTP.queryString req
          loc = if num == 0 then url <> arg else url
      -- logWarn "network error" ["count" .= num, "limit" .= retryLimit, "loc" .= loc, "failed" .= show @Text ctx]
      promIncrCounter httpFailureCounter label
      pure True
    InvalidUrlException _ _ -> pure False

-- | A retry helper with a constant policy. This helper is in charge of low level logging
-- and TODO: incrementCounter for graphql request and errors
constantRetry :: RetryEffect :> es => Text -> Handler (Eff es) Bool -> (Int -> Eff es a) -> Eff es a
constantRetry msg handler baseAction = retry policy (const handler) action
 where
  delay = 1_100_000 -- 1.1 seconds
  policy = Retry.constantDelay delay <> Retry.limitRetries retryLimit
  action num = do
    when (num > 0) $
      -- logWarn "Retry failed" ["num" .= num, "max" .= retryLimit, "msg" .= msg]
      pure ()
    baseAction num
