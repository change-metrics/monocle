{-# LANGUAGE TemplateHaskell #-}
-- undefined is needed in the Generic Selector
{-# OPTIONS_GHC -Wno-deprecations #-}
-- witch instance for Google.Protobuf.Timestamp
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An augmented relude with extra package such as time and aeson.
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    getEnv',
    setEnv,
    headMaybe,
    Secret (..),
    (:::),

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
    MonadError (..),
    MonadWriter (..),
    WriterT,
    runWriterT,

    -- * exceptions
    MonadThrow (..),
    MonadMask,
    MonadCatch (..),
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
    naturalToCount,

    -- * say
    say,
    sayErr,
    monocleLog,

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

    -- * qq-literals
    utctime,

    -- * lens
    Lens',
    lens,
    mapMOf,
    view,
    over,

    -- * aeson
    FromJSON (..),
    ToJSON (..),
    Value,
    encode,
    encodePretty,
    encodePrettyWithSpace,
    (.=),

    -- * bloodhound
    BH.MonadBH,
    BH.DocId,
    BH.BulkOperation (..),
    simpleSearch,
    doSearch,
    mkAnd,
    mkOr,
    mkNot,
    mkTerm,

    -- * proto3
    fromPBEnum,

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
  )
where

import Control.Exception.Safe (tryAny)
import qualified Control.Foldl as L
import Control.Lens (Lens', lens, mapMOf, over, view)
import Control.Monad.Catch (Handler (Handler), MonadCatch (catch), MonadMask, MonadThrow (throwM))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), encode, withText, (.=))
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Fixed (Deci, Fixed (..), HasResolution (resolution), Pico)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock (getCurrentTime)
import Data.Vector (Vector)
import qualified Database.Bloodhound as BH
import GHC.Float (double2Float)
import GHC.Generics (C, D, K1, M1, R, Rep, S, Selector, U1, selName, (:*:), (:+:))
import qualified Google.Protobuf.Timestamp
import Language.Haskell.TH.Quote (QuasiQuoter)
import Prometheus (Info (..), counter, incCounter, withLabel)
import qualified Prometheus
import Proto3.Suite (Enumerated (..))
import QQLiterals (qqLiteral)
import Relude
import Relude.Extra.Foldable (average)
import Relude.Extra.Group (groupBy)
import Say (say, sayErr)
import Streaming (Of (..))
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as S
import System.Environment (setEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.MVar (modifyMVar, modifyMVar_)
import Witch hiding (over)

-- | Prometheus
type CounterLabel = Prometheus.Vector (Text, Text, Text) Prometheus.Counter

promRegister :: MonadIO m => Prometheus.Metric s -> m s
promRegister = Prometheus.register

promVector :: Prometheus.Label l => l -> Prometheus.Metric m -> Prometheus.Metric (Prometheus.Vector l m)
promVector = Prometheus.vector

incrementCounter :: Prometheus.MonadMonitor m => CounterLabel -> "labels" ::: (Text, Text, Text) -> m ()
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

{-# NOINLINE httpRequestCounter #-}
httpRequestCounter :: CounterLabel
httpRequestCounter =
  unsafePerformIO $ promRegister $ promVector ("ident", "url", "type") $ Prometheus.counter (Info "http_request" "")

{-# NOINLINE httpFailureCounter #-}
httpFailureCounter :: CounterLabel
httpFailureCounter =
  unsafePerformIO $ promRegister $ promVector ("ident", "url", "type") $ counter (Info "http_failure" "")

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
assertEqual' :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
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

instance ToJSON MonocleTime where
  toJSON (MonocleTime utcTime) = String . toText . formatTime defaultTimeLocale "%FT%TZ" $ utcTime

instance FromJSON MonocleTime where
  parseJSON = withText "UTCTimePlus" (parse . toString)
    where
      oldFormat = "%FT%T"
      utcFormat = "%FT%TZ"
      tryParse f s = parseTimeM False defaultTimeLocale f s
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
  val <- toText . exceptEnv <$> lookupEnv (toString var)
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
formatTime' formatText = toText . formatTime defaultTimeLocale (toString formatText)

-- | Helper
parseDateValue :: String -> Maybe UTCTime
parseDateValue str = tryParse "%F" <|> tryParse "%F %T %Z"
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt str

-- | Numerical type to count documents
newtype Count = MkCount Word32
  deriving newtype (Show, Eq, Ord, Enum, Real, Integral, FromJSON)

countToWord :: Count -> Word32
countToWord (MkCount x) = x

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

-- | From https://hackage.haskell.org/package/astro-0.4.3.0/docs/src/Data.Astro.Utils.html#fromFixed
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = fromIntegral v / fromIntegral (resolution fv)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

getExn :: (ToText e, HasCallStack) => Either e a -> a
getExn (Right x) = x
getExn (Left err) = error (toText err)

monocleLog :: MonadIO m => Text -> m ()
monocleLog = sayErr

fromPBEnum :: Enumerated a -> a
fromPBEnum (Enumerated (Left x)) = error $ "Unknown enum value: " <> show x
fromPBEnum (Enumerated (Right x)) = x

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

-- | Helper search func that can be replaced by a scanSearch
doSearch :: (FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m (BH.SearchResult a)
doSearch indexName search = do
  -- monocleLog . decodeUtf8 . encode $ search
  rawResp <- BH.searchByIndex indexName search
  -- monocleLog $ show rawResp
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left _e -> handleError rawResp
    Right x -> pure x
  where
    handleError resp = do
      monocleLog (show resp)
      error "Elastic response failed"

simpleSearch :: (FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m [BH.Hit a]
simpleSearch indexName search = BH.hits . BH.searchHits <$> doSearch indexName search

mkAnd :: [BH.Query] -> BH.Query
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] (BH.Filter <$> andQ) [] []

mkOr :: [BH.Query] -> BH.Query
mkOr orQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [] orQ

mkNot :: [BH.Query] -> BH.Query
mkNot notQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] notQ []

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term name value) Nothing

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
