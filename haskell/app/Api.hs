{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import qualified Monocle.Api
import Options.Generic
import Relude

data CLIOptions w = CLIOption
  { elasticUrl :: w ::: Maybe String <?> "The Elasticsearch endpoint (or ELASTIC_CONN env), default to elastic:9200",
    config :: w ::: Maybe FilePath <?> "The Monocle configuration (or CONFIG env), default to /etc/monocle/config.yaml",
    port :: w ::: Maybe String <?> "The API port to listen to (or PORT env), default to 9898"
  }
  deriving stock (Generic)

instance ParseRecord (CLIOptions Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOptWithDefault :: Maybe String -> String -> String -> IO String
getOptWithDefault argOpt envName defaultValue = do
  fromEnv <- lookupEnv envName
  pure $ fromMaybe (fromMaybe defaultValue fromEnv) argOpt

getConfigOpt :: Maybe String -> IO String
getConfigOpt argOpt = getOptWithDefault argOpt "CONFIG" "/etc/monocle/config.yaml"

getURLOpt :: Maybe String -> IO Text
getURLOpt argOpt = do
  url <- getOptWithDefault argOpt "ELASTIC_CONN" "elastic:9200"
  let hasScheme = isPrefixOf "http://" url || isPrefixOf "https://" url
      url' = if hasScheme then url else "http://" <> url
  pure $ toText url'

getPortOpt :: Maybe String -> IO Int
getPortOpt argOpt = do
  port <- getOptWithDefault argOpt "PORT" "9898"
  pure $ fromMaybe (error "Invalid port") (readMaybe port)

main :: IO ()
main = do
  args <- unwrapRecord "Monocle API"
  config <- getConfigOpt (config args)
  url <- getURLOpt (elasticUrl args)
  port <- getPortOpt (port args)
  Monocle.Api.run port url config
