{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | The CLI entrpoints
module CLI (mainApi, mainMacroscope, mainLentille) where

import Env
import qualified Lentille.Gerrit as G
import Macroscope.Main (runMacroscope)
import qualified Monocle.Api
import Monocle.Client (withClient)
import Monocle.Prelude hiding ((:::))
import Monocle.Search.Query (parseDateValue)
import Options.Generic
import qualified Streaming.Prelude as S

---------------------------------------------------------------
-- API cli
---------------------------------------------------------------

data CliEnv = CliEnv
  { config :: String,
    elastic_conn :: String
  }

cliEnv :: IO CliEnv
cliEnv =
  Env.parse (header "monocle-api available environement variables") $
    CliEnv <$> var str "CONFIG" (help "The Monocle configuration" <> def "/etc/monocle/config.yaml" <> helpDef show)
      <*> var str "ELASTIC_CONN" (help "The Elasticsearch endpoint" <> def "elastic:9200")

newtype CLIOptions w = CLIOption
  { port :: w ::: Maybe Int <?> "The API port to listen to, default to 9898"
  }
  deriving stock (Generic)

instance ParseRecord (CLIOptions Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getURL :: String -> Text
getURL url =
  let hasScheme = isPrefixOf "http://" url || isPrefixOf "https://" url
      url' = if hasScheme then url else "http://" <> url
   in toText url'

mainApi :: IO ()
mainApi = do
  -- Fetch environement variables
  CliEnv {config, elastic_conn} <- cliEnv
  -- Run arguments parser
  CLIOption port' <- unwrapRecord "Monocle API"
  -- Run the Monocle API
  Monocle.Api.run (fromMaybe 8989 port') (getURL elastic_conn) config

---------------------------------------------------------------
-- Macroscope cli
---------------------------------------------------------------

data Macroscope w = Macroscope
  { monocleUrl :: w ::: Maybe Text <?> "The monocle API",
    port :: w ::: Int <!> "9001" <?> "Health check port",
    config :: w ::: Maybe FilePath <?> "The monocle configuration"
  }
  deriving stock (Generic)

instance ParseRecord (Macroscope Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

mainMacroscope :: IO ()
mainMacroscope = do
  Macroscope monocleUrl' port' config'' <- unwrapRecord "Macroscope lentille runner"
  config' <- fromMaybe (error "--config or CONFIG env is required") <$> lookupEnv "CONFIG"
  withClient (fromMaybe "http://web:8080" monocleUrl') Nothing $ \client ->
    runMacroscope
      port'
      (fromMaybe config' config'')
      client

---------------------------------------------------------------
-- Lentille cli
---------------------------------------------------------------

data Lentille
  = GerritChange {url :: Text, change :: Int}
  | GerritProjects {url :: Text, query :: Text}
  | GerritChanges {url :: Text, project :: Text, since :: Text, limit :: Maybe Int}
  deriving stock (Generic)

instance ParseRecord Lentille where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

dump :: (MonadCatch m, MonadIO m, ToJSON a) => Maybe Int -> Stream (Of a) m () -> m ()
dump limitM stream = do
  xsE <- tryAny $ S.toList_ $ brk stream
  case xsE of
    Right xs -> liftIO . putBSLn . from . encodePrettyWithSpace 2 $ xs
    Left _ -> liftIO . putBSLn $ "Couldn't evalue the stream"
  liftIO . putLBSLn =<< exportMetricsAsText
  where
    brk = maybe id S.take limitM

mainLentille :: IO ()
mainLentille = do
  args <- getRecord "Lentille runner"
  case args of
    GerritChange url change -> do
      env <- getGerritEnv url
      dump Nothing $ G.streamChange env [G.ChangeId $ show change]
    GerritProjects url query -> do
      env <- getGerritEnv url
      dump Nothing $ G.streamProject env $ G.Regexp query
    GerritChanges url project since limit -> do
      env <- getGerritEnv url
      dump limit $ G.streamChange env [G.Project project, G.After (toSince since)]
  where
    toSince txt = case Monocle.Search.Query.parseDateValue txt of
      Just x -> x
      Nothing -> error $ "Invalid date: " <> show txt
    getGerritEnv url = do
      client <- G.getGerritClient url Nothing
      pure $ G.GerritEnv client Nothing (const Nothing) "cli"
