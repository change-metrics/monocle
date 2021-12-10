-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import qualified Control.Concurrent (modifyMVar, newMVar, threadDelay)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import qualified Data.Text as T
import qualified Data.Time.Clock (getCurrentTime)
import Monocle.Client (MonocleClient)
import Monocle.Client.Api (crawlerAddDoc, crawlerCommit, crawlerCommitInfo)
import Monocle.Crawler (AddDocRequest, AddDocResponse, CommitInfoRequest, CommitInfoResponse, CommitRequest, CommitResponse)
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
  let delaySec = diffUTCTimeToSec resetTime currentTime + 1
  mThreadDelay $ delaySec * 1_000_000
  where
    diffUTCTimeToSec a b =
      truncate (realToFrac . nominalDiffTimeToSeconds $ diffUTCTime a b :: Double) :: Int

-------------------------------------------------------------------------------
-- A concurrent system handled via Control.Concurrent.MVar (Unlifted from IO)

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

data LogCrawlerContext = LogCrawlerContext {index :: Text, crawler :: Text}

data LogEvent
  = LogMacroStart
  | LogStartingMonitoring Int
  | LogMacroPause Word32
  | LogMacroStartCrawlers [Text]
  | LogMacroContinue LogCrawlerContext
  | LogMacroSkipCrawler LogCrawlerContext Text
  | LogMacroStartCrawler LogCrawlerContext
  | LogMacroPostData LogCrawlerContext Text Int
  | LogMacroRequestOldestEntity LogCrawlerContext Text
  | LogMacroGotOldestEntity LogCrawlerContext (Text, Text) UTCTime
  | LogMacroNoOldestEnity LogCrawlerContext
  | LogMacroEnded LogCrawlerContext
  | LogMacroCommitFailed LogCrawlerContext
  | LogMacroPostDataFailed LogCrawlerContext [Text]
  | LogMacroStreamError LogCrawlerContext Text
  | LogMacroGroupStart Text
  | LogMacroGroupEnd Text
  | LogMacroReloadingStart
  | LogNetworkFailure Text
  | LogGetBugs UTCTime Int Int
  | LogGraphQL LogCrawlerContext Text
  | LogRaw Text

instance From LogEvent Text where
  from = \case
    LogMacroStart -> "Starting to fetch streams"
    LogStartingMonitoring port -> "Starting monitoring service on port " <> show port
    LogMacroPause usec -> "Waiting " <> show usec <> " sec. brb"
    LogMacroStartCrawlers xs -> "Starting " <> show (length xs) <> " threads for " <> show xs
    LogMacroContinue lc -> prefix lc <> " - Continuing on next entity"
    LogMacroSkipCrawler lc err -> prefix lc <> " - Skipping due to an unexpected exception catched: " <> err
    LogMacroStartCrawler lc -> prefix lc <> " - Start crawling entities"
    LogMacroPostData lc eName count -> prefix lc <> " - Posting " <> show count <> " documents to: " <> eName
    LogMacroRequestOldestEntity lc entity -> prefix lc <> " - Looking for oldest refreshed " <> entity <> " entity"
    LogMacroGotOldestEntity lc (etype, name) date ->
      prefix lc <> " - Got entity of type: " <> etype <> " named: " <> name <> " last updated at " <> show date
    LogMacroNoOldestEnity lc -> prefix lc <> " - Unable to find entity to update"
    LogMacroEnded lc -> prefix lc <> " - Crawling entities completed"
    LogMacroCommitFailed lc -> prefix lc <> " - Commit date failed"
    LogMacroPostDataFailed lc errors -> prefix lc <> " - Post documents failed: " <> T.intercalate " | " errors
    LogMacroStreamError lc error' -> prefix lc <> " - Error occured when consuming the document stream: " <> error'
    LogNetworkFailure msg -> "Network error: " <> msg
    LogGetBugs ts offset limit ->
      "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
    LogMacroGroupStart name -> "Group start: " <> name
    LogMacroGroupEnd name -> "Group end: " <> name
    LogMacroReloadingStart -> "Macroscope reloading beging"
    LogGraphQL lc text -> prefix lc <> " - " <> text
    LogRaw t -> t
    where
      prefix LogCrawlerContext {..} = "[" <> index <> "] " <> "Crawler: " <> crawler

class (Monad m, MonadTime m) => MonadLog m where
  mLog :: Log -> m ()

instance MonadLog IO where
  mLog = sayErr . from

-------------------------------------------------------------------------------
-- A GraphQL client system

class (MonadRetry m, MonadLog m, MonadTime m, MonadSync m) => MonadGraphQL m where
  httpRequest :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LByteString)
  newManager :: m HTTP.Manager

instance MonadGraphQL IO where
  httpRequest = HTTP.httpLbs
  newManager = HTTP.newManager HTTP.defaultManagerSettings

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
  retry :: (Text, Text, Text) -> m a -> m a

instance MonadRetry IO where
  retry = retry'

-- | Retry 5 times network action, doubling backoff each time
-- Use this retry' to implement MonadRetry in IO.
retry' :: (MonadMask m, MonadLog m, MonadIO m, MonadMonitor m) => (Text, Text, Text) -> m a -> m a
retry' label baseAction =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 7)
    [handler]
    (const action)
  where
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
        mLog . Log Unspecified . LogNetworkFailure $ show num <> "/6 " <> loc <> " failed: " <> show ctx
        incrementCounter httpFailureCounter label
        pure True
      InvalidUrlException _ _ -> pure False
