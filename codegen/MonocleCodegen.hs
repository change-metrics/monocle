-- See https://www.schoolofhaskell.com/user/kseo/overloaded-string-literals
{-# LANGUAGE OverloadedStrings #-}
-- To use relude we need to disable the default standard library using NoImplicitPrelude
{-# LANGUAGE NoImplicitPrelude #-}

-- | A prototype to generate json encoder stubs from protobuf definition
module Main (main) where

import Data.List (lookup)
import qualified Data.Text as Text
import Language.ProtocolBuffers.Parser (parseProtoBufFile)
-- http://hackage.haskell.org/package/language-protobuf-1.0.1/docs/Language-ProtocolBuffers-Types.html
import qualified Language.ProtocolBuffers.Types as PB
-- http://hackage.haskell.org/package/relude
import Relude
-- http://hackage.haskell.org/package/casing-0.1.4.1/docs/Text-Casing.html
import qualified Text.Casing as Casing

-- A convenient helper to manage generated file content
fromProto ::
  -- | File headers
  [Text] ->
  -- | Create a service stub (service name, [(method name, input, output, path)])
  (Text -> [(Text, [Text], [Text], Text)] -> [Text]) ->
  -- | The protobuf input
  PB.ProtoBuf ->
  -- | The codegen output
  String
fromProto headers mkService pb =
  toString
    ( unlines
        (headers <> concatMap mkServiceDecl (PB.services pb) <> [])
    )
  where
    mkServiceDecl :: PB.ServiceDeclaration -> [Text]
    mkServiceDecl pbService = case pbService of
      PB.Service name _ methods -> [""] <> mkService name (map methodIO methods)
    methodIO :: PB.Method -> (Text, [Text], [Text], Text)
    methodIO pbMethod = case pbMethod of
      PB.Method name _ (PB.TOther input) _ (PB.TOther output) opts -> (name, input, output, getPath opts)
      _ -> error "Invalid method"
    getPath :: [PB.Option] -> Text
    getPath = foldr (flip go) ""
      where
        go "" (PB.Option ["google", "api", "http"] (PB.KObject xs)) =
          case lookup "post" xs of
            Just (PB.KString path) -> path
            x -> error ("Unknown path" <> show x)
        go verb _ = verb

intercalateText :: String -> [Text] -> Text
intercalateText sep = toText . intercalate sep . map toString

snake :: Text -> Text
snake = toText . Casing.quietSnake . toString

attrName :: [Text] -> Text
attrName [_, name] = name
attrName x = error ("Invalid attr name: " <> show x)

-- | Create python modules from a protobuf definition
protoToPython :: PB.ProtoBuf -> String
protoToPython = fromProto headers mkService
  where
    headers =
      [ "# Copyright (C) 2021 Monocle authors",
        "# SPDX-License-Identifier: AGPL-3.0-or-later",
        "",
        "# Generated by monocle-codegen. DO NOT EDIT!",
        "",
        "from flask import request",
        "from google.protobuf import json_format as pbjson",
        ""
      ]
    mkService _ [] = []
    mkService name methods =
      [ "def " <> snake name <> "_service(app):",
        "  from monocle.api import " <> intercalateText ", " (map (snake . getName) methods)
      ]
        <> concatMap typeImport methods
        <> [""]
        <> concatMap (mkMethod name) methods
      where
        getName (methodName, _, _, _) = name <> "_" <> methodName
        typeImport (_, input, output, _) =
          [ "  from monocle.messages." <> moduleName input <> "_pb2 import " <> attrName input,
            "  from monocle.messages." <> moduleName output <> "_pb2 import " <> attrName output
          ]
        moduleName [package, _] = Text.drop (Text.length "monocle_") package
        moduleName x = error ("Invalid module name: " <> show x)

    mkMethod serviceName (name, input, output, path) =
      [ "  def " <> snake name <> "_stub() -> None:",
        "    input_data: bytes = request.get_data() or b\"{}\"",
        "    input_request: " <> attrName input <> input_req <> "  # type: ignore",
        "    output_resp: " <> attrName output <> " = " <> snake (serviceName <> name) <> "(input_request)",
        "    json_resp = pbjson.MessageToJson(output_resp, preserving_proto_field_name=True)",
        "    return app.response_class(" <> intercalateText ", " resp_args <> ")",
        "  app.add_url_rule(" <> intercalateText ", " route_args <> ")",
        ""
      ]
      where
        input_req = " = pbjson.Parse(input_data," <> attrName input <> "())"
        resp_args = ["response=json_resp", "status=200", "mimetype=\"application/json\""]
        route_args =
          [ "\"" <> path <> "\"",
            "\"" <> name <> "\"",
            snake name <> "_stub",
            "methods=[\"GET\", \"POST\"]"
          ]

-- | Create rescript modules from a protobuf definition
protoToReScript :: PB.ProtoBuf -> String
protoToReScript = fromProto headers mkService
  where
    headers =
      [ "// Copyright (C) 2021 Monocle authors",
        "// SPDX-License-Identifier: AGPL-3.0-or-later",
        "",
        "// Generated by monocle-codegen. DO NOT EDIT!",
        "",
        "type axiosResponse<'data> = {data: 'data}",
        "type axios<'data> = Js.Promise.t<axiosResponse<'data>>",
        "@module(\"../api.js\") external serverUrl: string = \"server\""
      ]
    mkService _ [] = []
    mkService name methods =
      ["module " <> name <> " = {"]
        <> concatMap (mkMethod name) methods
        <> ["}"]

    camel :: Text -> Text
    camel = toText . Casing.camel . toString

    msgName moduleName msg = moduleName <> "Types." <> snake (attrName msg)

    mkMethod moduleName (name, input, output, path) =
      [ "@module(\"axios\")",
        "external " <> camel name <> "Raw: (string, 'a) => axios<'b> = \"post\"",
        "",
        methodDef <> "=>",
        requestEncode <> " |> " <> requestCall <> " |> " <> promiseDecode
      ]
      where
        methodDef = "let " <> camel name <> " = (" <> methodInput <> "): " <> methodOutput
        methodInput = "request: " <> msgName moduleName input
        methodOutput = "axios<" <> msgName moduleName output <> ">"
        requestEncode = "request->" <> moduleName <> "Bs.encode_" <> snake (attrName input)
        requestCall = camel name <> "Raw(serverUrl ++ \"" <> path <> "\")"
        promiseDecode = "Js.Promise.then_(resp => { data: " <> decodeResponse <> "}->Js.Promise.resolve)"
        decodeResponse = "resp.data-> " <> moduleName <> "Bs.decode_" <> snake (attrName output)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [protoFile, pythonFile, rescriptFile] -> do
      pbE <- parseProtoBufFile protoFile
      case pbE of
        Left err -> error $ show err
        Right pb -> do
          writeFile pythonFile (protoToPython pb)
          writeFile rescriptFile (protoToReScript pb)
    _ -> putTextLn "usage: monocle-codegen input.proto output.py output.res"
