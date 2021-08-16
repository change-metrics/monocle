{-# LANGUAGE PackageImports #-}

-- | Extra functions for json-syntax
module Json.Extras
  ( -- * Data types
    module Json,
    ShortText,
    TextShort.toText,

    -- * Extras
    decodeThrow,
    getAttr,
    getArray,
    getString,
    getDate,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Bytes as Bytes
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TextShort
import qualified Data.Text.Time as TextTime
import Data.Time.Clock (UTCTime)
import "json-syntax" Json
import Relude

decodeThrow :: LBS.ByteString -> Json.Value
decodeThrow dat = case decode (Bytes.fromByteString (toStrict dat)) of
  Left e -> error $ "Could not decode: " <> (show e)
  Right v -> v

-- | 'getAttr' return an object value
getAttr :: ShortText -> Json.Value -> Maybe Json.Value
getAttr k v = case v of
  Json.Object xs -> getFirst $ foldMap getValue' xs
  _ -> Nothing
  where
    getValue' Json.Member {..}
      | key == k = First $ Just value
      | otherwise = First Nothing

getString :: Json.Value -> Maybe ShortText
getString v = case v of
  Json.String x -> pure x
  _ -> Nothing

getArray :: Json.Value -> Maybe [Json.Value]
getArray v = case v of
  Json.Array xs -> pure $ toList xs
  _ -> Nothing

getDate :: Json.Value -> Maybe UTCTime
getDate v = case v of
  Json.String t -> either (const Nothing) Just $ TextTime.parseUTCTimeOrError (TextShort.toText t)
  _ -> error "Not a date"
