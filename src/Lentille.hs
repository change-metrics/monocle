-- | A shared library between lentilles and macroscope
module Lentille (
  -- * The lentille context
  LentilleM (..),
  CrawlerEnv (..),
  LentilleStream,
  LentilleMonad,
  runLentilleM,
  stopLentille,
  unlessStopped,

  -- * Lentille Errors
  MonadGraphQLE,
  LentilleError (..),
  RequestLog (..),
  LogCrawlerContext (..),

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
  swapDuration,

  -- * Re-export
  module Monocle.Class,
) where

import Data.Text qualified as T
import Google.Protobuf.Timestamp qualified as T
import Monocle.Class
import Monocle.Client (MonocleClient, baseUrl, mkManager)
import Monocle.Config qualified as Config
import Monocle.Logging
import Monocle.Prelude
import Monocle.Protob.Change (
  Change (changeUpdatedAt),
  ChangeEvent,
  ChangeEventOptionalDuration (ChangeEventOptionalDurationDuration),
  ChangeOptionalDuration (ChangeOptionalDurationDuration),
  Change_ChangeState (Change_ChangeStateClosed, Change_ChangeStateMerged),
  Ident (..),
 )
import Network.HTTP.Client qualified as HTTP
import Proto3.Suite (Enumerated (Enumerated))

-------------------------------------------------------------------------------
-- The Lentille context

newtype LentilleM a = LentilleM {unLentille :: ReaderT CrawlerEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadReader CrawlerEnv)
  deriving newtype (MonadUnliftIO, MonadMonitor)

instance HasLogger LentilleM where
  getLogger = asks crawlerLogger
  logIO = liftIO

data CrawlerEnv = CrawlerEnv
  { crawlerClient :: MonocleClient
  , crawlerLogger :: Logger
  , crawlerStop :: IORef Bool
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

data RequestLog = RequestLog
  { rlRequest :: HTTP.Request
  , rlRequestBody :: LByteString
  , rlResponse :: HTTP.Response LByteString
  , rlResponseBody :: LByteString
  }
  deriving (Show)

data LentilleError
  = DecodeError [Text]
  | GetRateLimitError (Text, HTTP.Request, HTTP.Response LByteString)
  | -- | GraphQLError is a wrapper around the morpheus's FetchError.
    -- TODO: keep the original error data type (instead of the Text)
    GraphQLError (Text, RequestLog)
  deriving (Show)

instance Exception LentilleError

-- | Here we create the different class instance by using the LentilleM inner IO
instance MonadTime LentilleM where
  mGetCurrentTime = liftIO mGetCurrentTime
  mThreadDelay de = liftIO $ mThreadDelay de

instance MonadSync LentilleM where
  mNewMVar = newMVar
  mModifyMVar = modifyMVar

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

instance Config.MonadConfig LentilleM where
  mReloadConfig fp = do
    reloader <- liftIO $ Config.reloadConfig fp
    pure $ liftIO reloader
  mGetSecret def = liftIO . Config.getSecret def

type LentilleStream m a = Stream (Of a) m ()

type LentilleMonad m =
  ( MonadTime m
  , MonadCrawler m -- for monocle crawler http api
  , Config.MonadConfig m
  )

-------------------------------------------------------------------------------
-- Utility functions for crawlers
-------------------------------------------------------------------------------

getChangeId :: Text -> Text -> LText
getChangeId fullName iid = from . stripSpaces $ T.replace "/" "@" fullName <> "@" <> from iid

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
  identUid = from uid
  identMuid = from $ fromMaybe username (cb uid)

ghostIdent :: Text -> Ident
ghostIdent host = toIdent host (const Nothing) nobody

isChangeTooOld :: UTCTime -> (Change, [ChangeEvent]) -> Bool
isChangeTooOld date (change, _) =
  case changeUpdatedAt change of
    Just changeDate -> T.toUTCTime changeDate < date
    _ -> True

swapDuration :: ChangeOptionalDuration -> ChangeEventOptionalDuration
swapDuration (ChangeOptionalDurationDuration v) = ChangeEventOptionalDurationDuration v
