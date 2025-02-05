{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.Adapter where

import Data.Morpheus.Client
import Data.Text qualified as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime)
import Google.Protobuf.Timestamp qualified as T
import Lentille (ghostIdent, nobody, toIdent)
import Monocle.Config qualified as Config
import Monocle.Prelude
import Monocle.Protob.Change
import Proto3.Suite (Enumerated (..))
import Relude

newtype Time = Time Text deriving (Show, Eq, FromJSON)

data DiffStatsSummary = DiffStatsSummary
  { additions :: Int
  , deletions :: Int
  , fileCount :: Int
  }

data DiffStatsSummaryItem = DSSAdditions | DSSDeletions | DSSFileCount

data DiffStats = DiffStats
  { path :: Text
  , additions :: Int
  , deletions :: Int
  }

newtype MRUserCore = MRUserCore {username :: Text}

data MRCommit = MRCommit
  { sha :: Text
  , cauthor :: Maybe MRUserCore
  , authoredDate :: Maybe Time
  , ctitle :: Maybe Text
  }

data CommentType = CoApproval Text | CoComment | CoOther deriving (Show)

data MRComment = MRComment
  { coId :: Text
  , coAuthor :: Ident
  , coAuthoredAt :: Time
  , coType :: CommentType
  }
  deriving (Show)

-- Some default data

defaultTimestamp :: Time
defaultTimestamp = Time "1970-01-01T00:00:00+00:00"

-- Generic utility fonction
fromMTtoLT :: From s LText => Maybe s -> LText
fromMTtoLT = maybe "" from

timeToTimestamp :: Time -> T.Timestamp
timeToTimestamp = T.fromUTCTime . timeToUTCTime

timeToUTCTime :: Time -> UTCTime
timeToUTCTime (Time t) =
  case parseDateValue (from t) of
    Nothing -> error $ "Unknown time format: " <> from t
    Just utc -> utc

cleanMaybeMNodes :: Maybe [Maybe a] -> [a]
cleanMaybeMNodes nodes = catMaybes $ fromMaybe [] nodes

--

getDSS :: Maybe DiffStatsSummary -> DiffStatsSummaryItem -> Int
getDSS dssM item =
  case dssM of
    Just DiffStatsSummary {..} -> case item of
      DSSAdditions -> additions
      DSSDeletions -> deletions
      DSSFileCount -> fileCount
    Nothing -> 0

getChangedFile :: DiffStats -> ChangedFile
getChangedFile DiffStats {..} = ChangedFile (from additions) (from deletions) (from path)

getChangeNumber :: Text -> Int32
getChangeNumber iid =
  from $ fromMaybe 0 ((readMaybe $ from iid) :: Maybe Int)

toCommit :: Text -> (Text -> Maybe Config.IdentUG) -> MRCommit -> Commit
toCommit host cb MRCommit {..} =
  Commit
    (from sha)
    (Just . toIdent' $ getAuthor cauthor)
    (Just . toIdent' $ getAuthor cauthor)
    (Just . timeToTimestamp $ fromMaybe defaultTimestamp authoredDate)
    (Just . timeToTimestamp $ fromMaybe defaultTimestamp authoredDate)
    0
    0
    (from $ fromMaybe "" ctitle)
 where
  getAuthor :: Maybe MRUserCore -> Text
  getAuthor (Just MRUserCore {..}) = username
  getAuthor Nothing = nobody
  toIdent' = toIdent host cb

toState :: Text -> Enumerated Change_ChangeState
toState state' = case state' of
  "closed" -> Enumerated (Right Change_ChangeStateClosed)
  "merged" -> Enumerated $ Right Change_ChangeStateMerged
  "opened" -> Enumerated $ Right Change_ChangeStateOpen
  _otherwise -> error ("Unable to decode Merge Request state: " <> _otherwise)

isComment :: MRComment -> Bool
isComment MRComment {..} = case coType of
  CoComment -> True
  _ -> False

isApprovalComment :: MRComment -> Bool
isApprovalComment MRComment {..} = case coType of
  CoApproval _ -> True
  _ -> False
