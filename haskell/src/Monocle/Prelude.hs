-- | An augmented relude with extra package such as time and aeson.
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    MonadThrow,
    MonadMask,

    -- * say
    sayErr,
    monocleLog,

    -- * time
    UTCTime,
    getCurrentTime,

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

import Control.Monad.Catch (MonadMask, MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, encode)
import Data.Fixed (Fixed (..), HasResolution (resolution))
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Database.Bloodhound as BH
import GHC.Float (double2Float)
import Proto3.Suite (Enumerated (..))
import Relude
import Say (sayErr)

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
  -- monocleLog . decodeUtf8 . Aeson.encode $ search
  rawResp <- BH.searchByIndex indexName search
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
mkAnd andQ = BH.QueryBoolQuery $ BH.mkBoolQuery andQ [] [] []

mkOr :: [BH.Query] -> BH.Query
mkOr orQ = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [] orQ

mkTerm :: Text -> Text -> BH.Query
mkTerm name value = BH.TermQuery (BH.Term name value) Nothing
