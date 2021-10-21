{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Env
import qualified Monocle.Api
import Options.Generic
import Relude

---------------------------------------------------------------
-- CLI available environment variables
---------------------------------------------------------------

data CliEnv = CliEnv
  { config :: String,
    elastic_conn :: String
  }

cliEnv :: IO CliEnv
cliEnv =
  Env.parse (header "monocle-api available environement variables") $
    CliEnv <$> var (str) "CONFIG" (help "The Monocle configuration" <> def "/etc/monocle/config.yaml" <> helpDef show)
      <*> var (str) "ELASTIC_CONN" (help "The Elasticsearch endpoint" <> def "elastic:9200")

---------------------------------------------------------------
-- CLI arguments parser
---------------------------------------------------------------

data CLIOptions w = CLIOption
  { port :: w ::: Maybe Int <?> "The API port to listen to, default to 9898"
  }
  deriving stock (Generic)

instance ParseRecord (CLIOptions Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

---------------------------------------------------------------
-- CLI functions
---------------------------------------------------------------

getURL :: String -> Text
getURL url =
  let hasScheme = isPrefixOf "http://" url || isPrefixOf "https://" url
      url' = if hasScheme then url else "http://" <> url
   in toText url'

main :: IO ()
main = do
  -- Fetch environement variables
  CliEnv {config, elastic_conn} <- cliEnv
  -- Run arguments parser
  args <- unwrapRecord "Monocle API"
  -- Run the Monocle API
  Monocle.Api.run (fromMaybe 8989 (port args)) (getURL elastic_conn) config
