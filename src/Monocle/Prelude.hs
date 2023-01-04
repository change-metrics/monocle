{-# LANGUAGE TemplateHaskellQuotes #-}
-- undefined is needed in the Generic Selector
{-# OPTIONS_GHC -Wno-deprecations #-}
-- witch instance for Google.Protobuf.Timestamp
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An augmented relude with extra package such as time and aeson.
module Monocle.Prelude (
  module Relude,
  module GHC.Stack,
  fromFixed,
  double2Float,
  orDie,
  getExn,
  getEnv',
  setEnv,
  headMaybe,
  Secret (..),
  (:::),

  -- * logging
  module Monocle.Logging,

  -- * effectful
  Effectful.Retry.Retry,
  Effectful.Retry.runRetry,
  Effectful.Prometheus.PrometheusEffect,
  Effectful.Eff,
  Effectful.IOE,
  Effectful.Error.Static.Error,
  Effectful.Error.Static.runErrorNoCallStack,
  Effectful.Error.Static.throwError,
  runErrorIO,
  Effectful.Concurrent.Concurrent,
  Effectful.Concurrent.runConcurrent,
  Effectful.Effect,
  (:>>),
  (:>),
  Effectful.runEff,
  Effectful.withEffToIO,
  Effectful.runPureEff,
  Effectful.Dispatch.Static.unEff,
  Effectful.Dispatch.Static.unsafeEff,
  Effectful.Dispatch.Static.unsafeEff_,
  Effectful.Fail.Fail,
  Effectful.Fail.runFail,
  Effectful.Fail.runFailIO,

  -- * generic
  selectors,
  FromJSONField,
  Rep,
  AnyJSON,
  Selectors,

  -- * containers
  mapMutate,

  -- * witch
  From (..),
  into,
  unsafeFrom,
  unsafeInto,
  via,
  TryFrom (..),
  tryInto,
  tryVia,
  Witch.Encoding.UTF_8,
  Data.Tagged.Tagged (..),

  -- * streaming
  Stream,
  Of (..),
  toVector,

  -- * unliftio
  MonadUnliftIO,
  modifyMVar,
  modifyMVar_,
  withAsync,
  cancel,

  -- * mmoprh
  hoist,

  -- * mtl
  MonadWriter (..),
  WriterT,
  runWriterT,

  -- * exceptions
  Handler (Handler),
  tryAny,

  -- * tests
  Assertion,
  assertEqual,
  assertEqual',
  assertBool,
  assertFailure,

  -- * relude extra
  groupBy,
  average,

  -- * data.fixed
  Fixed (..),
  Deci,
  Pico,

  -- * custom numerical newtype
  Count,
  countToWord,
  countToDeci,
  wordToCount,
  naturalToCount,

  -- * time
  UTCTime,
  Monocle.Prelude.getCurrentTime,
  addUTCTime,
  elapsedSeconds,
  nominalDiffTimeToSeconds,
  diffUTCTime,
  formatTime',
  parseDateValue,
  dropTime,
  dropMilliSec,
  MonocleTime,
  toMonocleTime,
  diffTimeSec,

  -- * text
  stripSpaces,
  inText,
  getPath,

  -- * qq-literals
  utctime,

  -- * lens
  Lens',
  lens,
  mapMOf,
  view,
  over,
  preview,
  at,
  set,

  -- * lens-aeson
  _Integer,
  _Object,

  -- * aeson
  FromJSON (..),
  ToJSON (..),
  Value (Number),
  Pair,
  encode,
  encodePretty,
  encodePrettyWithSpace,
  (.=),
  object,

  -- * http-client-openssl
  withOpenSSL,
  newOpenSSLManager,

  -- * bloodhound
  BH.MonadBH,
  BH.DocId,
  BH.BulkOperation (..),
  mkAnd,
  mkOr,
  mkNot,
  mkTerm,
  BH.runBH,

  -- * proto3
  fromPBEnum,
  toPBEnum,

  -- * prometheus re-exports
  Prometheus.MonadMonitor (..),
  Prometheus.withLabel,
  Prometheus.incCounter,
  Prometheus.counter,
  Prometheus.exportMetricsAsText,
  Prometheus.Info (..),

  -- * promehteus re-exports using alias to avoid conflicts
  CounterLabel,
  promRegister,
  promVector,

  -- * global metrics
  incrementCounter,
  httpRequestCounter,
  httpFailureCounter,
  monocleQueryCheckCounter,
  monocleQueryCounter,
  monocleMetricCounter,
  monocleAuthSuccessCounter,
  monocleAuthProviderRedirectCounter,
) where

import Control.Exception.Safe (tryAny)
import Control.Foldl qualified as L
import Control.Lens (Lens', at, lens, mapMOf, over, preview, set, view)
import Control.Monad.Catch (Handler (Handler))
import Control.Monad.Morph (hoist)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Number, String), encode, object, withText, (.=))
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.Lens (_Integer, _Object)
import Data.Aeson.Types (Pair)
import Data.Fixed (Deci, Fixed (..), HasResolution (resolution), Pico)
import Data.Map qualified as Map
import Data.Tagged
import Data.Text qualified as T
import Data.Text.Internal.Search
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Data.Vector (Vector)
import Database.Bloodhound qualified as BH
import Effectful
import Effectful.Concurrent qualified
import Effectful.Dispatch.Static qualified
import Effectful.Error.Static qualified
import Effectful.Fail qualified
import Effectful.Prometheus qualified
import Effectful.Retry (Retry, runRetry)
import GHC.Float (double2Float)
import GHC.Generics (C, D, K1, M1, R, Rep, S, Selector, U1, selName, (:*:), (:+:))
import GHC.Stack
import Google.Protobuf.Timestamp qualified
import Language.Haskell.TH.Quote (QuasiQuoter)
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
import Prometheus (Info (..), counter, incCounter, withLabel)
import Prometheus qualified
import Proto3.Suite (Enumerated (..))
import QQLiterals (qqLiteral)
import Relude hiding (toLText, toLazy, toStrict, toString, toText)
import Relude.Extra.Foldable (average)
import Relude.Extra.Group (groupBy)
import Streaming (Of (..))
import Streaming.Prelude (Stream)
import Streaming.Prelude qualified as S
import System.Environment (setEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.MVar (modifyMVar, modifyMVar_)
import Witch hiding (over)
import Witch.Encoding

import Monocle.Logging

-- | Prometheus
type CounterLabel = Prometheus.Vector (Text, Text) Prometheus.Counter

promRegister :: MonadIO m => Prometheus.Metric s -> m s
promRegister = Prometheus.register

promVector :: Prometheus.Label l => l -> Prometheus.Metric m -> Prometheus.Metric (Prometheus.Vector l m)
promVector = Prometheus.vector

incrementCounter :: Prometheus.MonadMonitor m => CounterLabel -> ("module" ::: Text, "url" ::: Text) -> m ()
incrementCounter x l = withLabel x l incCounter

-------------------------------------------------------------------------------
-- Global metrics
{-# NOINLINE monocleQueryCheckCounter #-}
monocleQueryCheckCounter :: Prometheus.Counter
monocleQueryCheckCounter =
  unsafePerformIO $ promRegister $ Prometheus.counter (Info "query_check" "")

{-# NOINLINE monocleQueryCounter #-}
monocleQueryCounter :: Prometheus.Counter
monocleQueryCounter =
  unsafePerformIO $ promRegister $ Prometheus.counter (Info "query" "")

{-# NOINLINE monocleAuthSuccessCounter #-}
monocleAuthSuccessCounter :: Prometheus.Counter
monocleAuthSuccessCounter =
  unsafePerformIO $ promRegister $ Prometheus.counter (Info "auth_success" "")

{-# NOINLINE monocleAuthProviderRedirectCounter #-}
monocleAuthProviderRedirectCounter :: Prometheus.Counter
monocleAuthProviderRedirectCounter =
  unsafePerformIO $ promRegister $ Prometheus.counter (Info "auth_provider_redirect" "")

{-# NOINLINE monocleMetricCounter #-}
monocleMetricCounter :: Prometheus.Counter
monocleMetricCounter =
  unsafePerformIO $ promRegister $ Prometheus.counter (Info "metric" "")

{-# NOINLINE httpRequestCounter #-}
httpRequestCounter :: CounterLabel
httpRequestCounter =
  unsafePerformIO $ promRegister $ promVector ("module", "url") $ Prometheus.counter (Info "http_request" "")

{-# NOINLINE httpFailureCounter #-}
httpFailureCounter :: CounterLabel
httpFailureCounter =
  unsafePerformIO $ promRegister $ promVector ("module", "url") $ counter (Info "http_failure" "")

-------------------------------------------------------------------------------

-- | Annotate a type with a name (from https://hackage.haskell.org/package/vulkan-3.4/docs/Vulkan-NamedType.html)
type (name :: k) ::: a = a

-- | A newtype for secret like token pulled from the environment
newtype Secret = Secret {unSecret :: Text}
  deriving newtype (Eq, Ord, Hashable)

-- | Pretty json encoding with a fixed key order
encodePretty :: ToJSON a => a -> LByteString
encodePretty = encodePrettyWithSpace 0

-- | A lifted version of assertEqual
assertEqual' :: HasCallStack => (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual' n a b = liftIO $ assertEqual n a b

encodePrettyWithSpace :: ToJSON a => Int -> a -> LByteString
encodePrettyWithSpace space =
  Aeson.encodePretty'
    ( Aeson.defConfig {Aeson.confIndent = Aeson.Spaces space, Aeson.confCompare = compare @Text}
    )

-- | An helper to mutate a map using a monadic value
mapMutate :: (Ord k, Monad m) => Map k v -> k -> m v -> m (v, Map k v)
mapMutate m key mkValue =
  case Map.lookup key m of
    Just value ->
      -- The value was found, just return it
      pure (value, m)
    Nothing -> do
      -- Create a new value, store and return it
      value <- mkValue
      pure (value, Map.insert key value m)

eitherParseUTCTime :: String -> Either String UTCTime
eitherParseUTCTime x = maybe (Left ("Failed to parse time " <> x)) Right (readMaybe (x <> " Z"))

utctime :: QuasiQuoter
utctime = qqLiteral eitherParseUTCTime 'eitherParseUTCTime

-- | dropTime ensures the encoded date does not have millisecond.
-- This actually discard hour differences
dropTime :: UTCTime -> UTCTime
dropTime (UTCTime day _sec) = UTCTime day 0

-- | A newtype for UTCTime which doesn't have milli second and tolerates a missing trailing 'Z' when decoding from JSON
newtype MonocleTime = MonocleTime UTCTime
  deriving stock (Show, Eq, Ord)

instance From Text AesonKey.Key where
  from = AesonKey.fromText

instance ToJSON MonocleTime where
  toJSON (MonocleTime utcTime) = String . from . formatTime defaultTimeLocale "%FT%TZ" $ utcTime

instance FromJSON MonocleTime where
  parseJSON = withText "UTCTimePlus" (parse . from)
   where
    oldFormat = "%FT%T"
    utcFormat = "%FT%TZ"
    tryParse = parseTimeM False defaultTimeLocale
    parse s = MonocleTime <$> (tryParse oldFormat s <|> tryParse utcFormat s)

toMonocleTime :: UTCTime -> MonocleTime
toMonocleTime = MonocleTime . dropMilliSec

-- | drop millisecond from UTCTime
dropMilliSec :: UTCTime -> UTCTime
dropMilliSec (UTCTime day sec) = UTCTime day (fromInteger $ truncate sec)

headMaybe :: [a] -> Maybe a
headMaybe xs = head <$> nonEmpty xs

getEnv' :: Text -> IO Text
getEnv' var = do
  val <- from . exceptEnv <$> lookupEnv (from var)
  return $! val
 where
  exceptEnv = fromMaybe (error $ "ERROR: Missing environment variable named " <> var)

-- | A lifted version of getCurrentTime
getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Data.Time.Clock.getCurrentTime

-- | Return the seconds elapsed between a and b
-- >>> elapsedSeconds [utctime|2000-01-01 00:00:00|] [utctime|2000-01-01 01:00:00|]
-- 3600.000000000000
elapsedSeconds :: UTCTime -> UTCTime -> Pico
elapsedSeconds a b = nominalDiffTimeToSeconds $ diffUTCTime b a

-- | Helper to format time without timezone
formatTime' :: Text -> UTCTime -> Text
formatTime' formatText = from . formatTime defaultTimeLocale (from formatText)

-- | Helper
parseDateValue :: String -> Maybe UTCTime
parseDateValue str =
  tryParse "%F"
    <|> tryParse "%F %T %Z"
    <|> tryParse "%FT%XZ"
 where
  tryParse fmt = parseTimeM False defaultTimeLocale fmt str

-- | diffTimeSec a - b
-- >>> diffTimeSec [utctime|2000-01-01 01:00:00|] [utctime|2000-01-01 00:00:00|]
-- 3600
diffTimeSec :: a ::: UTCTime -> b ::: UTCTime -> Int
diffTimeSec a b = truncate (realToFrac $ elapsedSeconds b a :: Double) :: Int

-- | Numerical type to count documents
newtype Count = MkCount Word32
  deriving newtype (Show, Eq, Ord, Enum, Real, Integral, FromJSON, ToJSON)

countToWord :: Count -> Word32
countToWord (MkCount x) = x

wordToCount :: Word32 -> Count
wordToCount = MkCount

countToDeci :: Count -> Deci
countToDeci (MkCount x) = fromInteger (toInteger x)

naturalToCount :: Natural -> Count
naturalToCount = MkCount . fromInteger . toInteger

-- | A special Num instance that prevent arithmetic underflow
instance Num Count where
  MkCount a - MkCount b
    | b > a = MkCount 0
    | otherwise = MkCount $ a - b

  MkCount a + MkCount b = MkCount $ a + b
  MkCount a * MkCount b = MkCount $ a * b
  signum (MkCount a) = MkCount $ signum a
  fromInteger x = MkCount $ fromInteger x
  abs x = x

instance From Int Int32 where
  from = fromInteger . toInteger

instance From Word32 Int where
  from = fromInteger . toInteger

-- | From https://hackage.haskell.org/package/astro-0.4.3.0/docs/src/Data.Astro.Utils.html#fromFixed
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = fromIntegral v / fromIntegral (resolution fv)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

getExn :: (From e Text, HasCallStack) => Either e a -> a
getExn (Right x) = x
getExn (Left err) = error (from err)

-- >>> stripSpaces "john doe "
-- "johndoe"
stripSpaces :: Text -> Text
stripSpaces = T.replace " " ""

-- | Concat two Text separated by '/'
-- >>> getPath "change-metrics/" "monocle"
-- "change-metrics/monocle"
getPath :: Text -> Text -> Text
getPath base sub = T.dropWhileEnd (== '/') base <> "/" <> sub

inText :: Text -> Text -> Bool
inText sub txt = case indices sub txt of
  [] -> False
  _ -> True

fromPBEnum :: Enumerated a -> a
fromPBEnum (Enumerated (Left x)) = error $ "Unknown enum value: " <> show x
fromPBEnum (Enumerated (Right x)) = x

toPBEnum :: a -> Enumerated a
toPBEnum = Enumerated . Right

-------------------------------------------------------------------------------
-- Streaming helpers

-- | 'toVector' is an efficient convertion of stream into a vector.
--   though we should be using a toChunkedVector :: Size -> Stream -> [Vector]
toVector :: L.PrimMonad m => Stream (Of a) m () -> m (Vector a)
toVector s = do
  res :> _ <- L.impurely S.foldM L.vectorM s
  pure res

-------------------------------------------------------------------------------
-- Bloodhound helpers

mkAnd :: [BH.Query] -> BH.Query
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] (BH.Filter <$> andQ) [] []

mkOr :: [BH.Query] -> BH.Query
mkOr orQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [] orQ

mkNot :: [BH.Query] -> BH.Query
mkNot notQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] notQ []

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term (from name) value) Nothing

-- | Orphan instance
instance From UTCTime Google.Protobuf.Timestamp.Timestamp where
  from = Google.Protobuf.Timestamp.fromUTCTime

instance From Google.Protobuf.Timestamp.Timestamp UTCTime where
  from = Google.Protobuf.Timestamp.toUTCTime

-- | A typeclass to extract the field (selectors) name of a record
-- adapted from https://stackoverflow.com/a/27818445
class Selectors rep where
  selectors :: Proxy rep -> [String]

-- | Create Selectors instances that works with records Rep
-- For another implementation, see https://gist.github.com/Gabriel439/fb85640a1359491ed427526281220938
instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selector s => Selectors (M1 S s (K1 R t)) where
  selectors _ = [selName (undefined :: M1 S s (K1 R t) ())]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

-- | For unary datatype (such as AnyJSON), there are no field
instance Selectors U1 where
  selectors _ = []

-- | For sum type (e.g. Aeson.Value), we don't know the fields
instance Selectors (a :+: b) where
  selectors _ = []

-- instance Selectors (D1 a b c) where
--   selectors _ = []

-- | A convenient constraint to require a FromJSON instance with the selectors function
-- to get the record fields name, used by elasticSearch query.
type FromJSONField a = (FromJSON a, Selectors (Rep a))

-- | A little hack to satisfy the FromJSON constraint when we don't need the value
-- for example when looking for hitDocId. In that situation, we ensure whatever the
-- searchHit is, we can decode it.
data AnyJSON = AnyJSON
  deriving (Generic, Show)

instance FromJSON AnyJSON where
  parseJSON _ = pure AnyJSON

instance From Text Value where
  from = String

runErrorIO :: Show err => Eff (Effectful.Error.Static.Error err : es) a -> Eff es a
runErrorIO action = do
  res <- Effectful.Error.Static.runErrorNoCallStack action
  case res of
    Left e -> error (show e)
    Right x -> pure x
