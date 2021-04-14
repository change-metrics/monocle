{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille
import Lentille.Bugzilla
import Options.Generic
import Relude

data LentilleCli w = LentilleCli
  { monocleUrl :: w ::: Text <?> "The monocle API",
    index :: w ::: Text <?> "The index name",
    crawlerName :: w ::: Text <?> "The name of the crawler",
    bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url"
  }
  deriving stock (Generic)

instance ParseRecord (LentilleCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (LentilleCli Unwrapped)

apiKeyEnv :: String
apiKeyEnv = "MONOCLE_API_KEY"

apiKeyEnvError :: String
apiKeyEnvError = error $ toText apiKeyEnv <> " environment not found"

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
          (ApiKey . toText $ apiKey)
          (IndexName . index $ args)
          (CrawlerName . crawlerName $ args)
          (TrackerDataFetcher (getBZData bzSession))
