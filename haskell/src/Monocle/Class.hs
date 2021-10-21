-- | Monocle simple effect system based on mtl and PandocMonad
module Monocle.Class where

import qualified Control.Concurrent (threadDelay)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import qualified Data.Text as T
import qualified Data.Time.Clock (getCurrentTime)
import Monocle.Client (MonocleClient)
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
  | LogMacroPause Float
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
  | LogNetworkFailure Text
  | LogGetBugs UTCTime Int Int
  | LogRaw Text

instance From LogEvent Text where
  from = \case
    LogMacroStart -> "Starting to fetch streams"
    LogMacroPause usec -> "Waiting " <> show usec <> "s. brb"
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
    LogRaw t -> t
    where
      prefix LogCrawlerContext {..} = "[" <> index <> "] " <> "Crawler: " <> crawler

class (Monad m, MonadTime m) => MonadLog m where
  mLog :: Log -> m ()

instance MonadLog IO where
  mLog = sayErr . from

-------------------------------------------------------------------------------
-- A http system

class (MonadRetry m, MonadLog m) => MonadGraphQL m where
  httpRequest :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LByteString)
  newManager :: m (HTTP.Manager)

-------------------------------------------------------------------------------
-- The Monocle Crawler system

class Monad m => MonadCrawler m where
  mCrawlerAddDoc :: MonocleClient -> AddDocRequest -> m AddDocResponse
  mCrawlerCommit :: MonocleClient -> CommitRequest -> m CommitResponse
  mCrawlerCommitInfo :: MonocleClient -> CommitInfoRequest -> m CommitInfoResponse

-------------------------------------------------------------------------------
-- A network retry system

class Monad m => MonadRetry m where
  retry :: m a -> m a

instance MonadRetry IO where
  retry = retry'

-- | Retry 5 times network action, doubling backoff each time
-- Use this retry' to implement MonadRetry in IO.
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
        mLog . Log Unspecified . LogNetworkFailure $ show num <> "/6 " <> loc <> " failed: " <> show ctx
        pure True
      InvalidUrlException _ _ -> pure False
