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
import Lentille.Bugzilla
import Monocle.Client
import Monocle.Worker
import Options.Generic
import Relude
import qualified Streaming.Prelude as S

data LentilleCli w = LentilleCli
  { monocleUrl :: w ::: Text <?> "The monocle API",
    index :: w ::: Text <?> "The index name",
    crawlerName :: w ::: Text <?> "The name of the crawler",
    bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url",
    bugzillaProduct :: w ::: Text <?> "The bugzilla product name",
    since :: w ::: Maybe String <?> "Get bugs since",
    printBugs :: w ::: Bool <?> "Just print bugs, to not amend monocle"
  }
  deriving stock (Generic)

instance ParseRecord (LentilleCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (LentilleCli Unwrapped)

apiKeyEnv :: String
apiKeyEnv = "MONOCLE_API_KEY"

apiKeyEnvError :: String
apiKeyEnvError = error $ toText apiKeyEnv <> " environment not found"

bzKeyEnv :: String
bzKeyEnv = "BZ_API_KEY"

readSince :: Maybe String -> Maybe UTCTime
readSince = fmap (fromMaybe (error "Could not parse time") . readMaybe)

main :: IO ()
main = do
  args <- unwrapRecord "Lentille worker"
  apiKey <- fromMaybe apiKeyEnvError <$> lookupEnv apiKeyEnv
  bzKeyM <- fmap (BugzillaApikey . toText) <$> lookupEnv bzKeyEnv
  go args bzKeyM $! (toText apiKey)
  where
    go :: LentilleCli Unwrapped -> Maybe BugzillaApikey -> Text -> IO ()
    go args bzKeyM apiKey = do
      let bzUrl = fromMaybe "bugzilla.redhat.com" (bugzillaUrl args)
          sinceTSM = readSince $ since args
          sinceTS = fromMaybe (error "Couldn't parse since") sinceTSM
      bzSession <- getBugzillaSession bzUrl bzKeyM
      if printBugs args
        then S.mapM_ print (getBZData bzSession (bugzillaProduct args) sinceTS)
        else withClient (monocleUrl args) Nothing $ \client ->
          run
            client
            sinceTSM
            apiKey
            (index $ args)
            (crawlerName $ args)
            (TaskDataFetcher (getBZData bzSession (bugzillaProduct args)))
