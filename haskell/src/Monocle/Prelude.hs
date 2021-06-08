{-# LANGUAGE NoImplicitPrelude #-}

-- | Utility functions
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    MonadThrow,
    monocleLog,
    FromJSON (..),
    ToJSON (..),
    Value,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Fixed (Fixed (..), HasResolution (resolution))
import GHC.Float (double2Float)
import Relude
import Say (sayErr)

-- | From https://hackage.haskell.org/package/astro-0.4.3.0/docs/src/Data.Astro.Utils.html#fromFixed
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = fromIntegral v / fromIntegral (resolution fv)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> String -> Either String a
Just a `orDie` _ = Right a
Nothing `orDie` string = Left string

getExn :: Either String a -> a
getExn (Right x) = x
getExn (Left err) = error (toText err)

monocleLog :: MonadIO m => Text -> m ()
monocleLog = sayErr
