{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.Adapter where

import Data.Morpheus.Client
import qualified Data.Text as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Google.Protobuf.Timestamp as T
import Lentille (ghostIdent, nobody, toIdent)
import Monocle.Change
import Monocle.Prelude
import Proto3.Suite (Enumerated (..))
import Relude

newtype Time = Time Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

data DiffStatsSummary = DiffStatsSummary
  { additions :: Int,
    deletions :: Int,
    fileCount :: Int
  }

data DiffStatsSummaryItem = DSSAdditions | DSSDeletions | DSSFileCount

data DiffStats = DiffStats
  { path :: Text,
    additions :: Int,
    deletions :: Int
  }

newtype MRUserCore = MRUserCore {username :: Text}

data MRCommit = MRCommit
  { sha :: Text,
    cauthor :: Maybe MRUserCore,
    authoredDate :: Maybe Time,
    ctitle :: Maybe Text
  }

data CommentType = CoApproval Text | CoComment | CoOther deriving (Show)

data MRComment = MRComment
  { coId :: Text,
    coAuthor :: Ident,
    coAuthoredAt :: Time,
    coType :: CommentType
  }
  deriving (Show)

-- Some default data

commitFormatString :: Maybe String
commitFormatString = Just "%FT%X%EZ"

defaultTimestamp :: Time
defaultTimestamp = Time "1970-01-01T00:00:00+00:00"

-- Generic utility fonction

fromMTtoLT :: (LazyStrict l s, IsString s) => Maybe s -> l
fromMTtoLT t = toLazy $ fromMaybe "" t

timeToTimestamp :: Maybe String -> Time -> T.Timestamp
timeToTimestamp formatStringE = T.fromUTCTime . timeToUTCTime formatStringE

timeToUTCTime :: Maybe String -> Time -> UTCTime
timeToUTCTime formatStringE t =
  let Time tt = t
   in parseTimeOrError
        False
        defaultTimeLocale
        (fromMaybe "%FT%XZ" formatStringE)
        $ toString tt

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
getChangedFile DiffStats {..} = ChangedFile (from additions) (from deletions) (toLazy path)

getChangeNumber :: Text -> Int32
getChangeNumber iid =
  from $ fromMaybe 0 ((readMaybe $ toString iid) :: Maybe Int)

toCommit :: Text -> (Text -> Maybe Text) -> MRCommit -> Commit
toCommit host cb MRCommit {..} =
  Commit
    (toLazy sha)
    (Just . toIdent' $ getAuthor cauthor)
    (Just . toIdent' $ getAuthor cauthor)
    (Just . timeToTimestamp commitFormatString $ fromMaybe defaultTimestamp authoredDate)
    (Just . timeToTimestamp commitFormatString $ fromMaybe defaultTimestamp authoredDate)
    0
    0
    (toLazy $ fromMaybe "" ctitle)
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

isClosed :: Enumerated Change_ChangeState -> Bool
isClosed state' = case state' of
  Enumerated (Right Change_ChangeStateClosed) -> True
  _otherwise -> False

isComment :: MRComment -> Bool
isComment MRComment {..} = case coType of
  CoComment -> True
  _ -> False

isApprovalComment :: MRComment -> Bool
isApprovalComment MRComment {..} = case coType of
  CoApproval _ -> True
  _ -> False
