{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Time.Clock (UTCTime)
import Lentille
import Lentille.Bugzilla
import Options.Generic
import Relude
import qualified Streaming.Prelude as S

data LentilleCli w = LentilleCli
  { monocleUrl :: w ::: Text <?> "The monocle API",
    index :: w ::: Text <?> "The index name",
    crawlerName :: w ::: Text <?> "The name of the crawler",
    bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url",
    since :: w ::: Maybe String <?> "Get bugs since"
  }
  deriving stock (Generic)

instance ParseRecord (LentilleCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (LentilleCli Unwrapped)

apiKeyEnv :: String
apiKeyEnv = "MONOCLE_API_KEY"

apiKeyEnvError :: String
apiKeyEnvError = error $ toText apiKeyEnv <> " environment not found"

readSince :: Maybe String -> Maybe UTCTime
readSince = fmap (fromMaybe (error "Could not parse time") . readMaybe)

main :: IO ()
main = do
  apiKey <- fromMaybe apiKeyEnvError <$> lookupEnv apiKeyEnv
  go $! apiKey
  where
    go apiKey = do
      args <- unwrapRecord "Lentille worker"
      withClient (monocleUrl args) Nothing $ \client -> do
        let bzUrl = fromMaybe "bugzilla.redhat.com" (bugzillaUrl args)
        bzSession <- getBugzillaSession bzUrl
        run
          client
          (readSince $ since args)
          (ApiKey . toText $ apiKey)
          (IndexName . index $ args)
          (CrawlerName . crawlerName $ args)
          (TaskDataFetcher (getBZData bzSession))

{-
data BZCli w = BZCli
  { bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url",
    since :: w ::: String <?> "Get bugs since"
  }
  deriving stock (Generic)

instance ParseRecord (BZCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (BZCli Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Lentille BZ worker"
  let sinceTS = fromMaybe (error "Couldn't parse since") (readMaybe (since args))
      bzUrl = fromMaybe "bugzilla.redhat.com" (bugzillaUrl args)
  bzSession <- getBugzillaSession bzUrl
  S.mapM_ print (getBZData bzSession sinceTS)
-}
