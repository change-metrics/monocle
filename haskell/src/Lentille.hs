-- | A shared library between lentilles and macroscope
module Lentille
  ( -- * The lentille context
    LentilleM (..),
    CrawlerEnv (..),
    LentilleStream,
    LentilleMonad,
    MonadLog (..),
    runLentilleM,
    stopLentille,
    unlessStopped,

    -- * Lentille Errors
    MonadGraphQLE,
    LentilleError (..),

    -- * Log context
    LogEvent (..),
    LogAuthor (..),
    Log (..),
    LogCrawlerContext (..),
    logEvent,
    logRaw,

    -- * Facilities
    getClientBaseUrl,

    -- * Re-export
    module Monocle.Class,
  )
where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Monocle.Api.Config (MonadConfig (..))
import qualified Monocle.Api.Config
import Monocle.Class
import Monocle.Client (MonocleClient, baseUrl, mkManager)
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ReaderT CrawlerEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadReader CrawlerEnv)
  deriving newtype (MonadUnliftIO, MonadMonitor)

data CrawlerEnv = CrawlerEnv
  { crawlerClient :: MonocleClient,
    crawlerStop :: IORef Bool
  }

getClientBaseUrl :: MonadReader CrawlerEnv m => m Text
getClientBaseUrl = do
  env <- asks crawlerClient
  pure $ baseUrl env

-- | unlessStopped skips the action when the config is changed
unlessStopped :: MonadCrawler m => MonadReader CrawlerEnv m => m () -> m ()
unlessStopped action = do
  stopRef <- asks crawlerStop
  stopped <- mReadIORef stopRef
  unless stopped action

runLentilleM :: MonadIO m => MonocleClient -> LentilleM a -> m a
runLentilleM client lm = do
  r <- liftIO $ newIORef False
  liftIO . flip runReaderT (env r) . unLentille $ lm
  where
    env = CrawlerEnv client

stopLentille :: MonadThrow m => LentilleError -> LentilleStream m a
stopLentille = lift . throwM

data LentilleError
  = DecodeError [Text]
  | HttpError (Text, HTTP.Request, HTTP.Response LByteString)
  deriving (Show)

instance Exception LentilleError

-- | Here we create the different class instance by using the LentilleM inner IO
instance MonadTime LentilleM where
  mGetCurrentTime = liftIO mGetCurrentTime
  mThreadDelay de = liftIO $ mThreadDelay de

instance MonadLog LentilleM where
  mLog = logEvent

instance MonadRetry LentilleM where
  retry = retry'

instance MonadCrawler LentilleM where
  mReadIORef = liftIO . mReadIORef
  mCrawlerAddDoc client = liftIO . mCrawlerAddDoc client
  mCrawlerCommit client = liftIO . mCrawlerCommit client
  mCrawlerCommitInfo client = liftIO . mCrawlerCommitInfo client

instance MonadGraphQL LentilleM where
  httpRequest req = liftIO . HTTP.httpLbs req
  newManager = liftIO mkManager

type MonadGraphQLE m = (MonadGraphQL m, MonadThrow m)

instance MonadConfig LentilleM where
  mReloadConfig fp = do
    reloader <- liftIO $ Monocle.Api.Config.reloadConfig fp
    pure $ liftIO reloader
  mGetSecret def = liftIO . Monocle.Api.Config.getSecret def

type LentilleStream m a = Stream (Of a) m ()

-------------------------------------------------------------------------------
-- The BugZilla context

-- | LentilleMonad is an alias for a bunch of constaints
type LentilleMonad m =
  ( MonadTime m,
    MonadLog m, -- log is the monocle log facility
    MonadGraphQL m, -- for http worker
    MonadCrawler m, -- for monocle crawler http api
    MonadConfig m
  )

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------

logEvent :: (MonadTime m, MonadIO m) => Log -> m ()
logEvent x = do
  now <- mGetCurrentTime
  say $ "[" <> showTime now <> "] " <> from x
  where
    showTime now = toText . take 23 $ formatTime defaultTimeLocale "%F %T.%q" now

logRaw :: MonadLog m => Text -> m ()
logRaw text = mLog $ Log Unspecified (LogRaw text)
