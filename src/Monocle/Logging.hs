-- | Monocle log events
module Monocle.Logging where

import Data.Text qualified as T
import Monocle.Config qualified as Config
import Monocle.Entity
import Monocle.Prelude
import Monocle.Protob.Search (QueryRequest_QueryType (..))
import Monocle.Search.Query qualified as Q

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
  | AddingChange LText Int Int
  | AddingProject Text Text Int
  | AddingTaskData LText Int
  | UpdatingEntity LText Entity UTCTime
  | Searching QueryRequest_QueryType LText Q.Query
  | RefreshIndex Config.Index
  | OIDCCallbackCall (Maybe Text) (Maybe Text) (Maybe Text)
  | OIDCProviderTokenRequested Text
  | JWTCreated Text Text
  | JWTCreateFailed Text Text

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
    AddingChange crawler changes events ->
      from crawler <> " adding " <> show changes <> " changes with " <> show events <> " events"
    AddingProject crawler organizationName projects ->
      crawler <> " adding " <> show projects <> " changes for organization: " <> organizationName
    AddingTaskData crawler tds ->
      from crawler <> " adding " <> show tds
    UpdatingEntity crawler entity ts ->
      from crawler <> " updating " <> show entity <> " to " <> show ts
    Searching queryType queryText query ->
      let jsonQuery = decodeUtf8 . encode $ Q.queryGet query id Nothing
       in "searching " <> show queryType <> " with `" <> from queryText <> "`: " <> jsonQuery
    RefreshIndex index ->
      "Ensure workspace: " <> Config.getWorkspaceName index <> " exists and refresh crawlers metadata"
    OIDCCallbackCall errM codeM stateM ->
      "Received OIDC Callback: (err: " <> show errM <> " code: " <> show codeM <> " state: " <> show stateM <> ")"
    OIDCProviderTokenRequested content ->
      "Requested OIDC IdToken: " <> content
    JWTCreated mUid redirectUri -> "JSON Web Token created for mUid: " <> mUid <> ". Redirecting user to: " <> redirectUri
    JWTCreateFailed mUid err -> "JSON Web Token created failed for mUid: " <> mUid <> " due to: " <> err
   where
    prefix LogCrawlerContext {..} = "[" <> lccIndex <> "] " <> "Crawler: " <> lccName
