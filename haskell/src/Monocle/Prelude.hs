{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Utility functions
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    MonadThrow,
    MonadMask,
    monocleLog,
    FromJSON (..),
    ToJSON (..),
    Value,
    UTCTime,
    MonocleClient,
    getCurrentTime,

    -- * bloodhound
    BH.MonadBH,

    -- * System events
    MonocleEvent (..),
    monocleLogEvent,

    -- * Application context
    TenantM,
    getIndexName,
    getIndexConfig,
  )
where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Fixed (Fixed (..), HasResolution (resolution))
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Database.Bloodhound as BH
import GHC.Float (double2Float)
import Monocle.Api.Client.Internal (MonocleClient)
import Monocle.Servant.Env
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

data MonocleEvent
  = AddingChange BH.IndexName Int Int

eventToText :: MonocleEvent -> Text
eventToText ev = case ev of
  AddingChange (BH.IndexName index) changes events ->
    "Adding " <> show changes <> " changes with " <> show events <> " events to " <> index

monocleLogEvent :: MonadIO m => MonocleEvent -> m ()
monocleLogEvent = sayErr . eventToText
