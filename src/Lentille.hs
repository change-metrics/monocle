{-# LANGUAGE DeriveAnyClass #-}

-- | A shared library between lentilles and macroscope
module Lentille (
  -- * The lentille context
  CrawlerEnv (..),
  LentilleStream,
  unlessStopped,

  -- * Lentille Errors
  LentilleError (..),
  LentilleErrorKind (..),
  RequestLog (..),
  GraphQLError (..),
  yieldStreamError,
  fmapFetchError,

  -- * Facilities
  getChangeId,
  isMerged,
  isClosed,
  nobody,
  toIdent,
  ghostIdent,
  sanitizeID,
  swapDuration,

  -- * Stream helper
  streamDropBefore,
  Changes,

  -- * Re-export
  module Monocle.Class,
  module Monocle.Logging,
) where

import Data.Morpheus.Client (FetchError (..))
import Data.Text qualified as T
import Google.Protobuf.Timestamp qualified as T
import Monocle.Class
import Monocle.Client (MonocleClient)
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
import Streaming.Prelude qualified as S

import Effectful.Reader.Static qualified as E
import Monocle.Config qualified as Config

-------------------------------------------------------------------------------
-- The Lentille context

data CrawlerEnv = CrawlerEnv
  { crawlerClient :: MonocleClient
  , crawlerStop :: IORef Bool
  }

-- | unlessStopped skips the action when the config is changed
unlessStopped :: E.Reader CrawlerEnv :> es => Eff es () -> Eff es ()
unlessStopped action = do
  stopRef <- E.asks crawlerStop
  -- TODO: replace IORef with Concurrent TVar
  stopped <- unsafeEff_ (readIORef stopRef)
  unless stopped action

data RequestLog = RequestLog
  { rlRequest :: HTTP.Request
  , rlRequestBody :: LByteString
  , rlResponse :: HTTP.Response LByteString
  , rlResponseBody :: LByteString
  }
  deriving (Show)

instance ToJSON RequestLog where
  toJSON (RequestLog _ body _ resp) =
    object
      ["body" .= decodeUtf8 @Text body, "resp" .= decodeUtf8 @Text resp]

-- | ErrorGraphQL is a wrapper around the morpheus's FetchError.
data GraphQLError = GraphQLError
  { err :: FetchError ()
  , request :: RequestLog
  }
  deriving (Show, Generic)

fmapFetchError :: (a -> b) -> FetchError a -> FetchError b
fmapFetchError f = \case
  FetchErrorProducedErrors es Nothing -> FetchErrorProducedErrors es Nothing
  FetchErrorProducedErrors es (Just a) -> FetchErrorProducedErrors es (Just $ f a)
  FetchErrorNoResult -> FetchErrorNoResult
  FetchErrorParseFailure s -> FetchErrorParseFailure s

instance ToJSON GraphQLError where
  toJSON e = object ["request" .= e.request, "fetch_error" .= fetchError]
   where
    fetchError = case e.err of
      FetchErrorParseFailure s -> toJSON @Text $ "parse failure: " <> from s
      FetchErrorNoResult -> toJSON @Text "no result"
      FetchErrorProducedErrors es _ -> toJSON es

data LentilleError = LentilleError UTCTime LentilleErrorKind
  deriving (Show, Generic, ToJSON)

data LentilleErrorKind
  = DecodeError [Text]
  | RequestError GraphQLError
  | RateLimitInfoError GraphQLError
  | PartialErrors Value
  | EntityRemoved
  deriving (Show, Generic, ToJSON)

yieldStreamError :: TimeEffect :> es => LentilleErrorKind -> LentilleStream es a
yieldStreamError e = do
  now <- lift mGetCurrentTime
  S.yield (Left $ LentilleError now e)

type LentilleStream es a = Stream (Of (Either LentilleError a)) (Eff es) ()

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

toIdent :: Text -> (Text -> Maybe Config.IdentUG) -> Text -> Ident
toIdent host cb username =
  Ident
    { identUid
    , identMuid = from identMuid
    , identGroups = fromList $ from <$> identGroups
    }
 where
  uid = host <> "/" <> username
  identUid = from uid
  (identMuid, identGroups) = fromMaybe (username, mempty) (cb uid)

ghostIdent :: Text -> Ident
ghostIdent host = toIdent host (const Nothing) nobody

type Changes = (Change, [ChangeEvent])

-- | Drop oldest element
-- This transform the stream by adding a limit.
-- We don't care about the rest so we replace it with ()
-- See: https://hackage.haskell.org/package/streaming-0.2.4.0/docs/Streaming-Prelude.html#v:break
--
-- >>> let stream = yieldStreamError (DecodeError ["oops"])
-- >>> runEff $ runTime $ S.length_ $ streamDropBefore [utctime|2021-05-31 00:00:00|] stream
-- 1
streamDropBefore :: UTCTime -> LentilleStream es Changes -> LentilleStream es Changes
streamDropBefore untilDate = fmap (pure ()) . S.break (isChangeTooOld untilDate)

-- | Return False to keep the stream element.
isChangeTooOld :: UTCTime -> Either LentilleError (Change, [ChangeEvent]) -> Bool
isChangeTooOld _ (Left _) = False
isChangeTooOld untilDate (Right (change, _)) =
  case changeUpdatedAt change of
    Just changeDate -> T.toUTCTime changeDate < untilDate
    _ -> False

swapDuration :: ChangeOptionalDuration -> ChangeEventOptionalDuration
swapDuration (ChangeOptionalDurationDuration v) = ChangeEventOptionalDurationDuration v
