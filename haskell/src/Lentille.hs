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

    -- * Re-export
    module Monocle.Class,
  )
where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Monocle.Api.Config (MonadConfig (..))
import qualified Monocle.Api.Config
import Monocle.Class
import Monocle.Client (MonocleClient, mkManager)
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import Say (say)

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ReaderT CrawlerEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadReader CrawlerEnv)
  deriving newtype (MonadUnliftIO)

data CrawlerEnv = CrawlerEnv
  { crawlerClient :: MonocleClient
  }

runLentilleM :: MonadIO m => MonocleClient -> LentilleM a -> m a
runLentilleM client = liftIO . flip runReaderT env . unLentille
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
  mGetCurrentTime = liftIO $ mGetCurrentTime
  mThreadDelay de = liftIO $ mThreadDelay de

instance MonadLog LentilleM where
  mLog = logEvent

instance MonadRetry LentilleM where
  retry = retry'

instance MonadCrawler LentilleM where
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

-- | A type class for the Gerrit API
class
  ( MonadTime m,
    MonadLog m, -- log is the monocle log facility
    MonadGraphQL m, -- for http worker
    MonadCrawler m, -- for monocle crawler http api
    MonadConfig m
  ) =>
  LentilleMonad m

instance LentilleMonad LentilleM

instance LentilleMonad IO

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
