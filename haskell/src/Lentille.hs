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
    getChangeId,
    isMerged,
    isClosed,
    nobody,
    toIdent,
    ghostIdent,
    sanitizeID,
    isChangeTooOld,

    -- * Re-export
    module Monocle.Class,
  )
where

import qualified Data.Text as T
import qualified Google.Protobuf.Timestamp as T
import Monocle.Api.Config (MonadConfig (..))
import qualified Monocle.Api.Config
import Monocle.Change (Change (changeUpdatedAt), ChangeEvent, Change_ChangeState (Change_ChangeStateClosed, Change_ChangeStateMerged), Ident (..))
import Monocle.Class
import Monocle.Client (MonocleClient, baseUrl, mkManager)
import Monocle.Logging
import Monocle.Prelude
import qualified Network.HTTP.Client as HTTP
import Proto3.Suite (Enumerated (Enumerated))

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ReaderT CrawlerEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadReader CrawlerEnv)
  deriving newtype (MonadUnliftIO, MonadMonitor)

data CrawlerEnv = CrawlerEnv
  { crawlerClient :: MonocleClient,
    crawlerLogger :: Logger,
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

runLentilleM :: MonadIO m => Logger -> MonocleClient -> LentilleM a -> m a
runLentilleM logger client lm = do
  r <- liftIO $ newIORef False
  liftIO . flip runReaderT (env r) . unLentille $ lm
  where
    env = CrawlerEnv client logger

stopLentille :: MonadThrow m => LentilleError -> LentilleStream m a
stopLentille = lift . throwM

data LentilleError
  = DecodeError [Text]
  | GetRateLimitError (Text, HTTP.Request, HTTP.Response LByteString)
  deriving (Show)

instance Exception LentilleError

-- | Here we create the different class instance by using the LentilleM inner IO
instance MonadTime LentilleM where
  mGetCurrentTime = liftIO mGetCurrentTime
  mThreadDelay de = liftIO $ mThreadDelay de

instance MonadSync LentilleM where
  mNewMVar = newMVar
  mModifyMVar = modifyMVar

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

type LentilleMonad m =
  ( MonadTime m,
    MonadLog m, -- log is the monocle log facility
    MonadCrawler m, -- for monocle crawler http api
    MonadConfig m
  )

-------------------------------------------------------------------------------
-- Log system
-------------------------------------------------------------------------------

logEvent :: Log -> LentilleM ()
logEvent x = do
  logger <- asks crawlerLogger
  liftIO $ doLog logger message
  where
    message = encodeUtf8 @Text @ByteString . from $ x

logRaw :: MonadLog m => Text -> m ()
logRaw text = mLog $ Log Unspecified (LogRaw text)

-------------------------------------------------------------------------------
-- Utility functions for crawlers
-------------------------------------------------------------------------------

getChangeId :: Text -> Text -> LText
getChangeId fullName iid = toLazy . stripSpaces $ T.replace "/" "@" fullName <> "@" <> toText iid

isMerged :: Enumerated Change_ChangeState -> Bool
isMerged state' = case state' of
  Enumerated (Right Change_ChangeStateMerged) -> True
  _otherwise -> False

isClosed :: Enumerated Change_ChangeState -> Bool
isClosed state' = case state' of
  Enumerated (Right Change_ChangeStateClosed) -> True
  _otherwise -> False

sanitizeID :: Text -> Text
sanitizeID = T.replace ":" "@" . T.replace "/" "@"

nobody :: Text
nobody = "ghost"

toIdent :: Text -> (Text -> Maybe Text) -> Text -> Ident
toIdent host cb username = Ident {..}
  where
    uid = host <> "/" <> username
    identUid = toLazy uid
    identMuid = toLazy $ fromMaybe username (cb uid)

ghostIdent :: Text -> Ident
ghostIdent host = toIdent host (const Nothing) nobody

isChangeTooOld :: UTCTime -> (Change, [ChangeEvent]) -> Bool
isChangeTooOld date (change, _) =
  case changeUpdatedAt change of
    Just changeDate -> T.toUTCTime changeDate < date
    _ -> True
