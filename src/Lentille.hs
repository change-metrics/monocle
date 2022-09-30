{-# LANGUAGE DeriveAnyClass #-}

-- | A shared library between lentilles and macroscope
module Lentille (
  -- * The lentille context
  CrawlerEnv (..),
  LentilleStream,
  unlessStopped,

  -- * Lentille Errors
  LentilleError (..),
  RequestLog (..),
  GraphQLError (..),

  -- * Facilities
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
  module Monocle.Logging,
) where

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

import Effectful.Reader.Static qualified as E

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
  { -- TODO: keep the original error data type (instead of the Text)
    err :: Text
  , request :: RequestLog
  }
  deriving (Show, Generic, ToJSON)

data LentilleError
  = DecodeError [Text]
  | GraphError GraphQLError
  deriving (Show, Generic, ToJSON)

instance Exception LentilleError

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

toIdent :: Text -> (Text -> Maybe Text) -> Text -> Ident
toIdent host cb username = Ident {..}
 where
  uid = host <> "/" <> username
  identUid = from uid
  identMuid = from $ fromMaybe username (cb uid)

ghostIdent :: Text -> Ident
ghostIdent host = toIdent host (const Nothing) nobody

isChangeTooOld :: UTCTime -> Either LentilleError (Change, [ChangeEvent]) -> Bool
isChangeTooOld _ (Left _) = True
isChangeTooOld date (Right (change, _)) =
  case changeUpdatedAt change of
    Just changeDate -> T.toUTCTime changeDate < date
    _ -> True

swapDuration :: ChangeOptionalDuration -> ChangeEventOptionalDuration
swapDuration (ChangeOptionalDurationDuration v) = ChangeEventOptionalDurationDuration v
