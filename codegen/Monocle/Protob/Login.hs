{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module Monocle.Protob.Login where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Applicative qualified as Hs
import Control.DeepSeq qualified as Hs
import Control.Monad qualified as Hs
import Data.ByteString qualified as Hs
import Data.Coerce qualified as Hs
import Data.Int qualified as Hs (Int16, Int32, Int64)
import Data.List.NonEmpty qualified as Hs (NonEmpty (..))
import Data.Map qualified as Hs (Map, mapKeysMonotonic)
import Data.Proxy qualified as Proxy
import Data.String qualified as Hs (fromString)
import Data.Text.Lazy qualified as Hs (Text)
import Data.Vector qualified as Hs (Vector)
import Data.Word qualified as Hs (Word16, Word32, Word64)
import GHC.Enum qualified as Hs
import GHC.Generics qualified as Hs
import Google.Protobuf.Wrappers.Polymorphic qualified as HsProtobuf (
  Wrapped (..),
 )
import Proto3.Suite.Class qualified as HsProtobuf
import Proto3.Suite.DotProto qualified as HsProtobufAST
import Proto3.Suite.JSONPB ((.:), (.=))
import Proto3.Suite.JSONPB qualified as HsJSONPB
import Proto3.Suite.Types qualified as HsProtobuf
import Proto3.Wire qualified as HsProtobuf
import Proto3.Wire.Decode qualified as HsProtobuf (
  Parser,
  RawField,
 )
import Unsafe.Coerce qualified as Hs
import Prelude qualified as Hs

newtype LoginValidationRequest = LoginValidationRequest {loginValidationRequestUsername :: Hs.Text}
  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
instance (Hs.NFData LoginValidationRequest)
instance (HsProtobuf.Named LoginValidationRequest) where
  nameOf _ = Hs.fromString "LoginValidationRequest"
instance (HsProtobuf.HasDefault LoginValidationRequest)
instance (HsProtobuf.Message LoginValidationRequest) where
  encodeMessage
    _
    LoginValidationRequest {loginValidationRequestUsername} =
      ( HsProtobuf.encodeMessageField
          (HsProtobuf.FieldNumber 1)
          ( (Hs.coerce @Hs.Text @((HsProtobuf.String Hs.Text)))
              loginValidationRequestUsername
          )
      )
  decodeMessage _ =
    Hs.pure LoginValidationRequest
      <*> ( (HsProtobuf.coerceOver @((HsProtobuf.String Hs.Text)) @Hs.Text)
              ( HsProtobuf.at
                  HsProtobuf.decodeMessageField
                  (HsProtobuf.FieldNumber 1)
              )
          )
  dotProto _ =
    [ HsProtobufAST.DotProtoField
        (HsProtobuf.FieldNumber 1)
        (HsProtobufAST.Prim HsProtobufAST.String)
        (HsProtobufAST.Single "username")
        []
        ""
    ]
instance (HsJSONPB.ToJSONPB LoginValidationRequest) where
  toJSONPB (LoginValidationRequest f1) =
    HsJSONPB.object
      [ "username"
          .= ((Hs.coerce @Hs.Text @((HsProtobuf.String Hs.Text))) f1)
      ]
  toEncodingPB (LoginValidationRequest f1) =
    HsJSONPB.pairs
      [ "username"
          .= ((Hs.coerce @Hs.Text @((HsProtobuf.String Hs.Text))) f1)
      ]
instance (HsJSONPB.FromJSONPB LoginValidationRequest) where
  parseJSONPB =
    HsJSONPB.withObject
      "LoginValidationRequest"
      ( \obj ->
          Hs.pure LoginValidationRequest
            <*> ( (HsProtobuf.coerceOver @((HsProtobuf.String Hs.Text)) @Hs.Text)
                    (obj .: "username")
                )
      )
instance (HsJSONPB.ToJSON LoginValidationRequest) where
  toJSON = HsJSONPB.toAesonValue
  toEncoding = HsJSONPB.toAesonEncoding
instance (HsJSONPB.FromJSON LoginValidationRequest) where
  parseJSON = HsJSONPB.parseJSONPB
newtype LoginValidationResponse = LoginValidationResponse {loginValidationResponseResult :: (Hs.Maybe LoginValidationResponseResult)}
  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
instance (Hs.NFData LoginValidationResponse)
instance (HsProtobuf.Named LoginValidationResponse) where
  nameOf _ = Hs.fromString "LoginValidationResponse"
instance (HsProtobuf.HasDefault LoginValidationResponse)
instance (HsProtobuf.Message LoginValidationResponse) where
  encodeMessage
    _
    LoginValidationResponse {loginValidationResponseResult} =
      ( case loginValidationResponseResult of
          Hs.Nothing -> Hs.mempty
          Hs.Just x ->
            case x of
              LoginValidationResponseResultValidationResult y ->
                HsProtobuf.encodeMessageField
                  (HsProtobuf.FieldNumber 1)
                  (HsProtobuf.ForceEmit y)
      )
  decodeMessage _ =
    Hs.pure LoginValidationResponse
      <*> HsProtobuf.oneof
        Hs.Nothing
        [
          ( (HsProtobuf.FieldNumber 1)
          , Hs.pure
              (Hs.Just Hs.. LoginValidationResponseResultValidationResult)
              <*> HsProtobuf.decodeMessageField
          )
        ]
  dotProto _ = []
instance (HsJSONPB.ToJSONPB LoginValidationResponse) where
  toJSONPB (LoginValidationResponse f1) =
    HsJSONPB.object
      [ ( let
            encodeResult =
              ( case f1 of
                  Hs.Just (LoginValidationResponseResultValidationResult f1) ->
                    HsJSONPB.pair "validation_result" f1
                  Hs.Nothing -> Hs.mempty
              )
           in
            ( \options ->
                if HsJSONPB.optEmitNamedOneof options
                  then ("result" .= HsJSONPB.objectOrNull [encodeResult] options) options
                  else encodeResult options
            )
        )
      ]
  toEncodingPB (LoginValidationResponse f1) =
    HsJSONPB.pairs
      [ ( let
            encodeResult =
              ( case f1 of
                  Hs.Just (LoginValidationResponseResultValidationResult f1) ->
                    HsJSONPB.pair "validation_result" f1
                  Hs.Nothing -> Hs.mempty
              )
           in
            ( \options ->
                if HsJSONPB.optEmitNamedOneof options
                  then ("result" .= HsJSONPB.pairsOrNull [encodeResult] options) options
                  else encodeResult options
            )
        )
      ]
instance (HsJSONPB.FromJSONPB LoginValidationResponse) where
  parseJSONPB =
    HsJSONPB.withObject
      "LoginValidationResponse"
      ( \obj ->
          Hs.pure LoginValidationResponse
            <*> ( let
                    parseResult parseObj =
                      Hs.msum
                        [ Hs.Just Hs.. LoginValidationResponseResultValidationResult
                            <$> HsJSONPB.parseField parseObj "validation_result"
                        , Hs.pure Hs.Nothing
                        ]
                   in
                    (obj .: "result" Hs.>>= HsJSONPB.withObject "result" parseResult)
                      <|> (parseResult obj)
                )
      )
instance (HsJSONPB.ToJSON LoginValidationResponse) where
  toJSON = HsJSONPB.toAesonValue
  toEncoding = HsJSONPB.toAesonEncoding
instance (HsJSONPB.FromJSON LoginValidationResponse) where
  parseJSON = HsJSONPB.parseJSONPB
data LoginValidationResponse_ValidationResult
  = LoginValidationResponse_ValidationResultUnknownIdent
  | LoginValidationResponse_ValidationResultKnownIdent
  deriving (Hs.Show, Hs.Eq, Hs.Generic, Hs.NFData)
instance (HsProtobuf.Named LoginValidationResponse_ValidationResult) where
  nameOf _ = Hs.fromString "LoginValidationResponse_ValidationResult"
instance (HsProtobuf.HasDefault LoginValidationResponse_ValidationResult)
instance (Hs.Bounded LoginValidationResponse_ValidationResult) where
  minBound = LoginValidationResponse_ValidationResultUnknownIdent
  maxBound = LoginValidationResponse_ValidationResultKnownIdent
instance (Hs.Ord LoginValidationResponse_ValidationResult) where
  compare x y =
    Hs.compare
      (HsProtobuf.fromProtoEnum x)
      (HsProtobuf.fromProtoEnum y)
instance (HsProtobuf.ProtoEnum LoginValidationResponse_ValidationResult) where
  toProtoEnumMay 0 =
    Hs.Just LoginValidationResponse_ValidationResultUnknownIdent
  toProtoEnumMay 1 =
    Hs.Just LoginValidationResponse_ValidationResultKnownIdent
  toProtoEnumMay _ = Hs.Nothing
  fromProtoEnum LoginValidationResponse_ValidationResultUnknownIdent =
    0
  fromProtoEnum LoginValidationResponse_ValidationResultKnownIdent =
    1
instance (HsJSONPB.ToJSONPB LoginValidationResponse_ValidationResult) where
  toJSONPB x _ = HsJSONPB.enumFieldString x
  toEncodingPB x _ = HsJSONPB.enumFieldEncoding x
instance (HsJSONPB.FromJSONPB LoginValidationResponse_ValidationResult) where
  parseJSONPB (HsJSONPB.String "UnknownIdent") =
    Hs.pure LoginValidationResponse_ValidationResultUnknownIdent
  parseJSONPB (HsJSONPB.String "KnownIdent") =
    Hs.pure LoginValidationResponse_ValidationResultKnownIdent
  parseJSONPB v =
    HsJSONPB.typeMismatch
      "LoginValidationResponse_ValidationResult"
      v
instance (HsJSONPB.ToJSON LoginValidationResponse_ValidationResult) where
  toJSON = HsJSONPB.toAesonValue
  toEncoding = HsJSONPB.toAesonEncoding
instance (HsJSONPB.FromJSON LoginValidationResponse_ValidationResult) where
  parseJSON = HsJSONPB.parseJSONPB
instance (HsProtobuf.Finite LoginValidationResponse_ValidationResult)
newtype LoginValidationResponseResult
  = LoginValidationResponseResultValidationResult (HsProtobuf.Enumerated Monocle.Protob.Login.LoginValidationResponse_ValidationResult)
  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
instance (Hs.NFData LoginValidationResponseResult)
instance (HsProtobuf.Named LoginValidationResponseResult) where
  nameOf _ = Hs.fromString "LoginValidationResponseResult"
