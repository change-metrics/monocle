-- | Monocle log events
module Monocle.Logging where

import Data.Text qualified as T
import Monocle.Config qualified as Config
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

data Entity = Project Text | Organization Text | TaskDataEntity Text
  deriving (Eq, Show)

instance From Entity Text where
  from = \case
    Project _ -> "project"
    Organization _ -> "organization"
    TaskDataEntity _ -> "taskdata"

instance From Entity LText where
  from = via @Text

getEntityName :: Entity -> Text
getEntityName = \case
  Project n -> n
  Organization n -> n
  TaskDataEntity n -> n

data LogEvent
  = LogMacroStart
  | LogStartingMonitoring Int
  | LogMacroPause Word32
  | LogMacroStartCrawlers [Text]
  | LogMacroContinue LogCrawlerContext
  | LogMacroSkipCrawler LogCrawlerContext Text
  | LogMacroStartCrawler LogCrawlerContext
  | LogMacroPostData LogCrawlerContext Text Int
  | LogMacroRequestOldestEntity LogCrawlerContext Text
  | LogMacroGotOldestEntity LogCrawlerContext (Text, Text) UTCTime
  | LogMacroNoOldestEnity LogCrawlerContext
  | LogMacroEnded LogCrawlerContext
  | LogMacroCommitFailed LogCrawlerContext
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
  | SystemReady Int Int Text
  | AuthSystemReady Text
  | ReloadConfig FilePath
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
    LogMacroPostData lc eName count -> prefix lc <> " - Posting " <> show count <> " documents to: " <> eName
    LogMacroRequestOldestEntity lc entity -> prefix lc <> " - Looking for oldest refreshed " <> entity <> " entity"
    LogMacroGotOldestEntity lc (etype, name) date ->
      prefix lc <> " - Got entity of type: " <> etype <> " named: " <> name <> " last updated at " <> show date
    LogMacroNoOldestEnity lc -> prefix lc <> " - Unable to find entity to update"
    LogMacroEnded lc -> prefix lc <> " - Crawling entities completed"
    LogMacroCommitFailed lc -> prefix lc <> " - Commit date failed"
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
    SystemReady tenantCount port url ->
      "Serving " <> show tenantCount <> " tenant(s) on 0.0.0.0:" <> show port <> " with elastic: " <> url
    AuthSystemReady provider_name -> "Authentication provider (" <> provider_name <> ") ready"
    RefreshIndex index ->
      "Ensure workspace: " <> Config.getWorkspaceName index <> " exists and refresh crawlers metadata"
    ReloadConfig fp ->
      "Reloading " <> from fp
    OIDCCallbackCall errM codeM stateM ->
      "Received OIDC Callback: (err: " <> show errM <> " code: " <> show codeM <> " state: " <> show stateM <> ")"
    OIDCProviderTokenRequested content ->
      "Requested OIDC IdToken: " <> content
    JWTCreated mUid redirectUri -> "JSON Web Token created for mUid: " <> mUid <> ". Redirecting user to: " <> redirectUri
    JWTCreateFailed mUid err -> "JSON Web Token created failed for mUid: " <> mUid <> " due to: " <> err
   where
    prefix LogCrawlerContext {..} = "[" <> lccIndex <> "] " <> "Crawler: " <> lccName
