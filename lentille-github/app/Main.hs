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
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lentille.GitHub
import Lentille.GitHub.Issues
import Monocle.Api.Client
import Monocle.TaskData
import Options.Generic
import Relude
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

data LentilleCli w = LentilleCli
  { monocleUrl :: w ::: Text <?> "The monocle API",
    index :: w ::: Text <?> "The index name",
    crawlerName :: w ::: Text <?> "The name of the crawler",
    githubUrl :: w ::: Maybe Text <?> "The github url",
    since :: w ::: Maybe String <?> "Get issues since",
    repo :: w ::: Text <?> "Repo to get linked issue on",
    printBugs :: w ::: Bool <?> "Just print TaskData, to not amend monocle"
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
  args <- unwrapRecord "Lentille GitHub Issue worker"
  apiKey <- fromMaybe apiKeyEnvError <$> lookupEnv apiKeyEnv
  go args $! (toText apiKey)
  where
    go :: LentilleCli Unwrapped -> Text -> IO ()
    go args apiKey = do
      ghClient <- newGithubGraphClient ghUrl
      if printBugs args
        then S.mapM_ print (streamLinkedIssue ghClient (buildSearchQuery (repo args) sinceTS))
        else withClient (monocleUrl args) Nothing $ \client ->
          run
            client
            sinceTSM
            apiKey
            (index $ args)
            (crawlerName $ args)
            (TaskDataFetcher (ghTDF ghClient))
      where
        ghTDF :: MonadIO m => GitHubGraphClient -> UTCTime -> Stream (Of TaskData) m ()
        ghTDF ghClient since' = streamLinkedIssue ghClient (buildSearchQuery (repo args) since')
        buildSearchQuery :: Text -> UTCTime -> String
        buildSearchQuery repo' utctime =
          toString $
            unwords
              [ "repo:" <> repo',
                "updated:>=" <> toSimpleDate utctime
              ]
        toSimpleDate :: UTCTime -> Text
        toSimpleDate utctime = toText $ formatTime defaultTimeLocale "%F" utctime
        ghUrl = fromMaybe "https://api.github.com/graphql" (githubUrl args)
        sinceTSM = readSince $ since args
        sinceTS = fromMaybe (error "Couldn't parse since") sinceTSM
