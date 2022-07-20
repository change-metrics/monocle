{-# LANGUAGE ImportQualifiedPost #-}
-- See https://www.schoolofhaskell.com/user/kseo/overloaded-string-literals
{-# LANGUAGE OverloadedStrings #-}
-- To use relude we need to disable the default standard library using NoImplicitPrelude
{-# LANGUAGE NoImplicitPrelude #-}

-- | A prototype to generate json encoder stubs from protobuf definition
module Main (main) where

import Data.List (lookup)
import Data.Text qualified as Text
import Language.ProtocolBuffers.Parser (parseProtoBufFile)
-- http://hackage.haskell.org/package/language-protobuf-1.0.1/docs/Language-ProtocolBuffers-Types.html
import Language.ProtocolBuffers.Types qualified as PB
-- http://hackage.haskell.org/package/relude
import Relude
-- http://hackage.haskell.org/package/casing-0.1.4.1/docs/Text-Casing.html
import Text.Casing qualified as Casing

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
      PB.Method name _ (PB.TOther input) _ (PB.TOther output) opts ->
        (name, input, output, getPath opts)
      _ -> error "Invalid method"

getPath :: [PB.Option] -> Text
getPath = foldr (flip go) ""
  where
    go "" (PB.Option ["google", "api", "http"] (PB.KObject xs)) =
      case lookup "post" xs of
        Just (PB.KString path) -> path
        x -> error ("Unknown path" <> show x)
    go verb _ = verb

snake :: Text -> Text
snake = toText . Casing.quietSnake . toString

camel :: Text -> Text
camel = toText . Casing.camel . toString

attrName :: [Text] -> Text
attrName [_, name] = name
attrName x = error ("Invalid attr name: " <> show x)

-- | Create haskell module from a protobuf definition
protoToHaskell :: PB.ProtoBuf -> String
protoToHaskell = fromProto headers mkService
  where
    headers =
      [ "{-# OPTIONS_GHC -fno-warn-missing-export-lists -fno-warn-unused-imports #-}",
        "-- |",
        "-- Copyright: (c) 2021 Monocle authors",
        "-- SPDX-License-Identifier: AGPL-3.0-only",
        "",
        "-- Generated by monocle-codegen. DO NOT EDIT!",
        "module Monocle.Client.Api where",
        "import Control.Monad.Catch (MonadThrow)",
        "import Control.Monad.IO.Class (MonadIO)",
        "import Monocle.Client (MonocleClient, monocleReq)",
        "import Monocle.Protob.Auth",
        "import Monocle.Protob.Config",
        "import Monocle.Protob.Crawler",
        "import Monocle.Protob.Login",
        "import Monocle.Protob.Search",
        "import Monocle.Protob.Metric"
      ]
    mkService name = concatMap (mkMethod name)
    mkMethod serviceName (name, input, output, path) =
      [ "",
        functionName <> " :: " <> constrains <> attrName input <> " -> m " <> attrName output,
        functionName <> " = monocleReq \"" <> Text.drop 1 path <> "\""
      ]
      where
        constrains = "(MonadThrow m, MonadIO m) => MonocleClient -> "
        functionName = camel (serviceName <> name)

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
        "type axiosRequestHeaders = {\"Authorization\": string}",
        "type axiosRequestConfig = {headers: axiosRequestHeaders}",
        "type axiosResponse<'data> = {data: 'data}",
        "type axios<'data> = Js.Promise.t<axiosResponse<'data>>",
        "let serverUrl = %raw(`",
        "  (window.API_URL !== '__API_URL__' ? window.API_URL : process.env.REACT_APP_API_URL || '')",
        "`)"
      ]
    mkService _ [] = []
    mkService name methods =
      ["module " <> name <> " = {"]
        <> concatMap (mkMethod name) methods
        <> ["}"]

    msgName moduleName msg = moduleName <> "Types." <> snake (attrName msg)

    mkMethod moduleName (name, input, output, path)
      | "/crawler/" `Text.isInfixOf` path = []
      | otherwise =
          [ "@module(\"axios\")",
            "external " <> camel name <> "Raw: (string, 'data, option<axiosRequestConfig>) => axios<'b> = \"post\"",
            "",
            methodDef' <> "=>",
            requestCall' <> " |> " <> promiseDecode
          ]
      where
        methodOutput = "axios<" <> msgName moduleName output <> ">"
        requestEncode = "request->" <> moduleName <> "Bs.encode_" <> snake (attrName input)
        promiseDecode = "Js.Promise.then_(resp => { data: " <> decodeResponse <> "}->Js.Promise.resolve)"
        decodeResponse = "resp.data-> " <> moduleName <> "Bs.decode_" <> snake (attrName output)

        methodDef' = "let " <> camel name <> " = (" <> methodInput' <> "): " <> methodOutput
        methodInput' = "request: " <> msgName moduleName input <> ", " <> "token: option<string>"
        tokenDef = "token->Belt.Option.flatMap(jwt => {headers: {\"Authorization\": \"Bearer \" ++ jwt}}->Some),"
        requestCall' = camel name <> "Raw(serverUrl ++ \"" <> path <> "\"," <> requestEncode <> "," <> tokenDef <> ")"

-- | Create haskell servant module from a protobuf defnition
protoToServant :: PB.ProtoBuf -> String
protoToServant pb =
  toString (unlines (headers <> imports <> api <> server))
  where
    headers =
      [ "{-# LANGUAGE DataKinds #-}",
        "-- |",
        "-- Copyright: (c) 2021 Monocle authors",
        "-- SPDX-License-Identifier: AGPL-3.0-only",
        "",
        "-- Generated by monocle-codegen. DO NOT EDIT!",
        "module Monocle.Servant.HTTP (MonocleAPI, server) where",
        "",
        "import Monocle.Env",
        "import Monocle.Servant.PBJSON (PBJSON)",
        "import Servant",
        "import Servant.Auth.Server (Auth, JWT, Cookie)",
        "import Monocle.Api.Jwt (AuthenticatedUser)"
      ]

    imports = concatMap mkImport methods
      where
        mkImport (name, method, _) =
          [ "import Monocle.Api.Server (" <> camel name <> method <> ")",
            "import Monocle.Protob."
              <> name
              <> " ("
              <> (method <> "Request, ")
              <> (method <> "Response)")
          ]

    api =
      [ "type MonocleAPI =",
        Text.intercalate "\n :<|>" $ map mkApi methods
      ]
      where
        mkApi (moduleName, name, path) =
          "  "
            <> ( Text.intercalate " :> "
                   . map (\c -> "\"" <> c <> "\"")
                   . Text.split (== '/')
                   $ Text.drop (Text.length "/api/2/") path
               )
            <> " :> Auth '[JWT, Cookie] AuthenticatedUser :> ReqBody '[JSON] "
            <> ("Monocle.Protob." <> moduleName <> "." <> name <> "Request")
            <> " :> Post  '[PBJSON, JSON] "
            <> ("Monocle.Protob." <> moduleName <> "." <> name <> "Response")
    server =
      [ "server :: ServerT MonocleAPI AppM",
        "server =",
        Text.intercalate "\n :<|>" $ map mkServer methods
      ]
      where
        mkServer (name, method, _) = "  " <> camel name <> method

    methods :: [(Text, Text, Text)]
    methods = concatMap mkSimpleService (PB.services pb)
      where
        mkSimpleService (PB.Service name _ methods') =
          mapMaybe (toSimpleService name) methods'
        toSimpleService serviceName (PB.Method name _ _ _ _ opts)
          | Text.isPrefixOf "/api/2/" $ getPath opts = Just (serviceName, name, getPath opts)
          | otherwise = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [protoFile, haskellFile, servantFile, rescriptFile] -> do
      pbE <- parseProtoBufFile protoFile
      case pbE of
        Left err -> error $ show err
        Right pb -> do
          writeFile haskellFile (protoToHaskell pb)
          writeFile servantFile (protoToServant pb)
          writeFile rescriptFile (protoToReScript pb)
    _ -> putTextLn "usage: monocle-codegen input.proto output.hs servant.hs output.py output.res"
