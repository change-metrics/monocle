-- | Monocle log events
module Monocle.Logging where

import Data.Text qualified as T
import Monocle.Entity
import Monocle.Prelude

data LogCrawlerContext = LogCrawlerContext
  { lccIndex :: Text
  , lccName :: Text
  , lccEntity :: Maybe Entity
  }

noContext :: LogCrawlerContext
noContext = LogCrawlerContext "<direct>" "CLI" Nothing

data LogEvent
  = LogMacroStart
  | LogStartingMonitoring Int
  | LogMacroPause Word32
  | LogMacroStartCrawlers [Text]
  | LogMacroContinue LogCrawlerContext
  | LogMacroSkipCrawler LogCrawlerContext Text
  | LogMacroStartCrawler LogCrawlerContext
  | LogMacroPostData LogCrawlerContext Entity Int
  | LogMacroRequestOldestEntity LogCrawlerContext Text
  | LogMacroGotOldestEntity LogCrawlerContext Entity UTCTime
  | LogMacroNoOldestEnity LogCrawlerContext
  | LogMacroEnded LogCrawlerContext Entity UTCTime
  | LogMacroCommitFailed LogCrawlerContext Text
  | LogMacroPostDataFailed LogCrawlerContext [Text]
  | LogMacroStreamError LogCrawlerContext Text
  | LogMacroGroupStart Text
  | LogMacroGroupEnd Text
  | LogMacroReloadingStart
  | LogNetworkFailure Text
  | LogGetBugs UTCTime Int Int
  | LogGraphQL LogCrawlerContext Text
  | LogRaw Text

instance From LogEvent Text where
  from = \case
    LogMacroStart -> "Starting to fetch streams"
    LogStartingMonitoring port -> "Starting monitoring service on port " <> show port
    LogMacroPause usec -> "Waiting " <> show usec <> " sec. brb"
    LogMacroStartCrawlers xs -> "Starting " <> show (length xs) <> " threads for " <> show xs
    LogMacroContinue lc -> prefix lc <> " - Continuing on next entity"
    LogMacroSkipCrawler lc err -> prefix lc <> " - Skipping due to an unexpected exception catched: " <> err
    LogMacroStartCrawler lc -> prefix lc <> " - Start crawling entities"
    LogMacroPostData lc entity count -> prefix lc <> " - Posting " <> show count <> " documents to: " <> show entity
    LogMacroRequestOldestEntity lc entity -> prefix lc <> " - Looking for oldest refreshed " <> entity <> " entity"
    LogMacroGotOldestEntity lc entity date ->
      prefix lc <> " - Got entity: " <> show entity <> " last updated at " <> show date
    LogMacroNoOldestEnity lc -> prefix lc <> " - Unable to find entity to update"
    LogMacroEnded lc entity date -> prefix lc <> " - Crawling entities completed, oldest " <> show entity <> " last updated at " <> show date
    LogMacroCommitFailed lc msg -> prefix lc <> " - Commit date failed: " <> msg
    LogMacroPostDataFailed lc errors -> prefix lc <> " - Post documents failed: " <> T.intercalate " | " errors
    LogMacroStreamError lc error' -> prefix lc <> " - Error occured when consuming the document stream: " <> error'
    LogNetworkFailure msg -> "Network error: " <> msg
    LogGetBugs ts offset limit ->
      "Getting bugs from " <> show ts <> " offset " <> show offset <> " limit " <> show limit
    LogMacroGroupStart name -> "Group start: " <> name
    LogMacroGroupEnd name -> "Group end: " <> name
    LogMacroReloadingStart -> "Macroscope reloading beging"
    LogGraphQL lc text -> prefix lc <> " - " <> text
    LogRaw t -> t
   where
    prefix LogCrawlerContext {..} = "[" <> lccIndex <> "] " <> "Crawler: " <> lccName
