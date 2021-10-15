-- | A shared library between lentilles and macroscope
module Lentille
  ( -- * The lentille context
    LentilleM (..),
    LentilleStream,
    LentilleMonad,
    runLentilleM,
    stopLentille,

    -- * The lentille worker context
    MonadBZ (..),
    MonadGerrit (..),
    GerritEnv (..),
    MonadGraphQL (..),
    MonadCrawler (..),

    -- * Lentille Errors
    LentilleError (..),

    -- * Log context
    MonadTime (..),
    MonadLog (..),
    LogEvent (..),
    LogAuthor (..),
    Log (..),
    LogCrawlerContext (..),
    logEvent,
    logRaw,
    genLog,

    -- * Retry context
    MonadRetry (..),
    retry',
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError, throwError)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Gerrit as G
import Gerrit.Data.Change (GerritChange, GerritQuery)
import Gerrit.Data.Project (GerritProjectsMessage)
import Monocle.Api.Config (MonadConfig (..))
import qualified Monocle.Api.Config
import Monocle.Client (MonocleClient, mkManager)
import Monocle.Client.Api (crawlerAddDoc, crawlerCommit, crawlerCommitInfo)
import Monocle.Crawler (AddDocRequest, AddDocResponse, CommitInfoRequest, CommitInfoResponse, CommitRequest, CommitResponse)
import Monocle.Prelude
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as HTTP
import Say (say)
import Web.Bugzilla.RedHat (BugzillaSession)
import qualified Web.Bugzilla.RedHat as BZ (BugzillaContext, BugzillaServer, Request, newBugzillaContext, sendBzRequest)

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ExceptT LentilleError IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadError LentilleError)

instance MonadTime LentilleM where
  mGetCurrentTime = liftIO $ getCurrentTime
  mThreadDelay = liftIO . threadDelay

instance MonadRetry LentilleM where
  retry = retry'

instance MonadLog LentilleM where
  log' = logEvent

instance MonadCrawler LentilleM where
  mCrawlerAddDoc client = liftIO . crawlerAddDoc client
  mCrawlerCommit client = liftIO . crawlerCommit client
  mCrawlerCommitInfo client = liftIO . crawlerCommitInfo client

instance MonadBZ LentilleM where
  bzRequest req = liftIO . BZ.sendBzRequest req
  newContext = liftIO . BZ.newBugzillaContext

instance MonadGraphQL LentilleM where
  httpRequest req = liftIO . HTTP.httpLbs req
  newManager = liftIO mkManager
  mLookupEnv = liftIO . lookupEnv

instance MonadConfig LentilleM where
  mReloadConfig fp = do
    reloader <- liftIO $ Monocle.Api.Config.reloadConfig fp
    pure $ liftIO reloader
  mGetSecret def = liftIO . Monocle.Api.Config.getSecret def

data LentilleError
  = DecodeError [Text]
  deriving (Show)

type LentilleStream m a = Stream (Of a) m ()

runLentilleM :: MonadIO m => LentilleM a -> m (Either LentilleError a)
runLentilleM = liftIO . runExceptT . unLentille

stopLentille :: MonadError LentilleError m => LentilleError -> LentilleStream m a
stopLentille = throwError

class (MonadError LentilleError m, MonadLog m, MonadRetry m) => MonadCrawler m where
  mCrawlerAddDoc :: MonocleClient -> AddDocRequest -> m AddDocResponse
  mCrawlerCommit :: MonocleClient -> CommitRequest -> m CommitResponse
  mCrawlerCommitInfo :: MonocleClient -> CommitInfoRequest -> m CommitInfoResponse

-------------------------------------------------------------------------------
-- The BugZilla context

class (MonadRetry m, MonadLog m, MonadError LentilleError m) => MonadBZ m where
  bzRequest :: FromJSON bugs => BugzillaSession -> BZ.Request -> m bugs
  newContext :: BZ.BugzillaServer -> m BZ.BugzillaContext

class (MonadRetry m, MonadLog m, MonadError LentilleError m) => MonadGraphQL m where
  httpRequest :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LByteString)
  newManager :: m (HTTP.Manager)
  mLookupEnv :: String -> m (Maybe String)

-- | A type class for the Gerrit API
data GerritEnv = GerritEnv
  { -- | The Gerrit connexion client
    client :: G.GerritClient,
    -- | A project fullname prefix as defined in the Monocle configuration
    prefix :: Maybe Text,
    -- | The identity alias callback
    identAliasCB :: Maybe (Text -> Maybe Text)
  }

class (MonadRetry m, MonadLog m, MonadError LentilleError m) => MonadGerrit m where
  getGerritClient :: Text -> Maybe (Text, Text) -> m G.GerritClient
  getProjects :: GerritEnv -> Int -> G.GerritProjectQuery -> Maybe Int -> m GerritProjectsMessage
  queryChanges :: GerritEnv -> Int -> [GerritQuery] -> Maybe Int -> m [GerritChange]

instance MonadGerrit LentilleM where
  getGerritClient url = liftIO . G.getClient url
  getProjects env count query startM =
    liftIO $ G.getProjects count query startM (client env)
  queryChanges env count queries startM =
    liftIO $ G.queryChanges count queries startM (client env)

-- The final Lentille constraint
class
  ( MonadTime m,
    MonadLog m, -- log is the monocle log facility
    MonadError LentilleError m, -- error enable stream to produce error
    MonadBZ m, -- for bugzilla worker
    MonadGerrit m,
    MonadGraphQL m, -- for http worker
    MonadCrawler m, -- for monocle crawler http api
    MonadConfig m
  ) =>
  LentilleMonad m

instance LentilleMonad LentilleM

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------

data LogAuthor = Macroscope | Unspecified

data Log = Log {author :: LogAuthor, event :: LogEvent}

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
  | LogMacroEnded LogCrawlerContext
  | LogMacroCommitFailed LogCrawlerContext
  | LogMacroPostDataFailed LogCrawlerContext [Text]
  | LogMacroStreamError LogCrawlerContext Text
  | LogNetworkFailure Text
  | LogGetBugs UTCTime Int Int
  | LogRaw Text

logAuthorToText :: LogAuthor -> Text
logAuthorToText Macroscope = "Macroscope"
logAuthorToText Unspecified = ""

logEvent :: (MonadTime m, MonadIO m) => Log -> m UTCTime
logEvent Log {..} = do
  now <- mGetCurrentTime
  say $ "[" <> showTime now <> " - " <> logAuthorToText author <> "]: " <> evStr
  pure now
  where
    showTime now = toText . take 23 $ formatTime defaultTimeLocale "%F %T.%q" now
    prefix LogCrawlerContext {..} = "[" <> index <> "] " <> "Crawler: " <> crawler
    evStr = case event of
      LogMacroStart -> "Starting to fetch streams"
      LogMacroPause usec -> "Waiting " <> show usec <> "s. brb"
      LogMacroContinue lc -> prefix lc <> " - Continuing on next entity"
      LogMacroSkipCrawler lc err -> prefix lc <> " - Skipping due to an unexpected exception catched: " <> err
      LogMacroStartCrawler lc -> prefix lc <> " - Start crawling entities"
      LogMacroPostData lc eName count -> prefix lc <> " - Posting " <> show count <> " documents to: " <> eName
      LogMacroRequestOldestEntity lc entity -> prefix lc <> " - Looking for oldest refreshed " <> entity <> " entity"
      LogMacroGotOldestEntity lc (etype, name) date ->
        prefix lc <> " - Got entity of type: " <> etype <> " named: " <> name <> " last updated at " <> show date
      LogMacroEnded lc -> prefix lc <> " - Crawling entities completed"
      LogMacroCommitFailed lc -> prefix lc <> " - Commit date failed"
      LogMacroPostDataFailed lc errors -> prefix lc <> " - Post documents failed: " <> T.intercalate " | " errors
      LogMacroStreamError lc error' -> prefix lc <> " - Error occured when consuming the document stream: " <> error'
      LogNetworkFailure msg -> "Network error: " <> msg
      LogGetBugs ts offset limit ->
        "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
      LogRaw t -> t

genLog :: LogAuthor -> LogEvent -> Log
genLog = Log

class Monad m => MonadLog m where
  log' :: Log -> m UTCTime
  log :: Log -> m ()
  log = void . log'

instance MonadLog IO where
  log' = logEvent

logRaw :: MonadLog m => Text -> m ()
logRaw text = log $ genLog Unspecified (LogRaw text)

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
        log $ genLog Unspecified (LogNetworkFailure $ show num <> "/6 " <> loc <> " failed: " <> show ctx)
        pure True
      InvalidUrlException _ _ -> pure False
