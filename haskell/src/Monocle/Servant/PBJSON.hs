{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A custom mime render type for pbjson
-- The goal is to make the oneOf encoding compatible with the
-- common protobuf implementation.
module Monocle.Servant.PBJSON (PBJSON) where

import Data.Aeson.Encoding (fromEncoding)
import Data.Binary.Builder (toLazyByteString)
import Proto3.Suite.JSONPB (ToJSONPB, defaultOptions, optEmitNamedOneof, toEncodingPB)
import Servant.API.ContentTypes (Accept (..), MimeRender (..))

-- | PBJSON is a new data to be used in servant api definition
data PBJSON

instance Accept PBJSON where
  contentType _ = "application/json"

instance ToJSONPB a => MimeRender PBJSON a where
  mimeRender _ v = toLazyByteString $ fromEncoding $ toEncodingPB v (defaultOptions {optEmitNamedOneof = False})
