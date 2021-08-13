-- | An augmented relude with extra package such as time and aeson.
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    getEnv',
    headMaybe,

    -- * exceptions
    MonadThrow,
    MonadMask,

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
    sayErr,
    monocleLog,

    -- * time
    UTCTime,
    getCurrentTime,
    elapsedSeconds,
    nominalDiffTimeToSeconds,
    diffUTCTime,
    formatTime',

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

    -- * bloodhound
    BH.MonadBH,
    simpleSearch,
    doSearch,
    mkAnd,
    mkOr,
    mkTerm,

    -- * proto3
    fromPBEnum,
  )
where

import Control.Lens (Lens', lens, mapMOf, over, view)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, encode)
import Data.Fixed (Deci, Fixed (..), HasResolution (resolution), Pico)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Database.Bloodhound as BH
import GHC.Float (double2Float)
import Proto3.Suite (Enumerated (..))
import Relude
import Relude.Extra.Foldable (average)
import Relude.Extra.Group (groupBy)
import Say (sayErr)

headMaybe :: [a] -> Maybe a
headMaybe xs = head <$> nonEmpty xs

getEnv' :: Text -> IO Text
getEnv' var = do
  val <- toText . exceptEnv <$> lookupEnv (toString var)
  return $! val
  where
    exceptEnv = fromMaybe (error $ "ERROR: Missing environment variable named " <> var)

-- $setup
-- >>> let mkDate s = (fromMaybe (error "oops") $ readMaybe s) :: UTCTime

-- | Return the seconds elapsed between a and b
-- >>> elapsedSeconds (mkDate "2000-01-01 00:00:00 Z") (mkDate "2000-01-01 01:00:00 Z")
-- 3600.000000000000
elapsedSeconds :: UTCTime -> UTCTime -> Pico
elapsedSeconds a b = nominalDiffTimeToSeconds $ diffUTCTime b a

-- | Helper to format time without timezone
formatTime' :: Text -> UTCTime -> Text
formatTime' formatText = toText . formatTime defaultTimeLocale (toString formatText)

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

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term name value) Nothing
