-- | Utility functions
module Monocle.Prelude
  ( module Relude,
    fromFixed,
    double2Float,
    orDie,
    getExn,
    MonadThrow,
    MonadMask,
    monocleLog,
    FromJSON (..),
    ToJSON (..),
    Value,
    UTCTime,
    MonocleClient,
    getCurrentTime,

    -- * bloodhound
    BH.MonadBH,

    -- * Shared types
    Entity (..),

    -- * System events
    MonocleEvent (..),
    monocleLogEvent,

    -- * Application context
    TenantM,
    getIndexName,
    getIndexConfig,
    fromPBEnum,

    -- * Query context
    QueryM,
    runQueryM,
    getQuery,
    liftTenantM,
    getQueryBH,
    getQueryBHWithFlavor,
    withFilter,
  )
where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, encode)
import Data.Fixed (Fixed (..), HasResolution (resolution))
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Database.Bloodhound as BH
import GHC.Float (double2Float)
import Monocle.Api.Client.Internal (MonocleClient)
import qualified Monocle.Api.Config as Config
import Monocle.Search (QueryRequest_QueryType (..))
import Monocle.Search.Syntax
import Monocle.Servant.Env
import Proto3.Suite (Enumerated (..))
import Relude
import Say (sayErr)

-- | From https://hackage.haskell.org/package/astro-0.4.3.0/docs/src/Data.Astro.Utils.html#fromFixed
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = fromIntegral v / fromIntegral (resolution fv)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

getExn :: (ToText e, HasCallStack) => Either e a -> a
getExn (Right x) = x
getExn (Left err) = error (toText err)

monocleLog :: MonadIO m => Text -> m ()
monocleLog = sayErr

data Entity = Project {getName :: Text} | Organization {getName :: Text}
  deriving (Eq, Show)

data MonocleEvent
  = AddingChange LText Int Int
  | AddingProject Text Text Int
  | UpdatingEntity LText Entity UTCTime
  | Searching QueryRequest_QueryType LText Query

eventToText :: MonocleEvent -> Text
eventToText ev = case ev of
  AddingChange crawler changes events ->
    toStrict crawler <> " adding " <> show changes <> " changes with " <> show events <> " events"
  AddingProject crawler organizationName projects ->
    crawler <> " adding " <> show projects <> " changes for organization" <> organizationName
  UpdatingEntity crawler entity ts ->
    toStrict crawler <> " updating " <> show entity <> " to " <> show ts
  Searching queryType queryText query ->
    let jsonQuery = decodeUtf8 . encode $ queryBH query defaultQueryFlavor
     in "searching " <> show queryType <> " with `" <> toStrict queryText <> "`: " <> jsonQuery

monocleLogEvent :: MonocleEvent -> TenantM ()
monocleLogEvent ev = do
  tenant <- getIndexConfig
  sayErr $ Config.index tenant <> ": " <> eventToText ev

fromPBEnum :: Enumerated a -> a
fromPBEnum (Enumerated (Left x)) = error $ "Unknown enum value: " <> show x
fromPBEnum (Enumerated (Right x)) = x
