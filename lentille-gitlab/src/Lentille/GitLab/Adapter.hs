{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}

module Lentille.GitLab.Adapter where

import Data.Morpheus.Client
import qualified Data.Text as TE
import Data.Time.Clock
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Google.Protobuf.Timestamp as T
import Monocle.Change
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
  { coAuthor :: Ident,
    coAuthoredAt :: Time,
    coType :: CommentType
  }
  deriving (Show)

-- Some default data

commitFormatString :: Maybe String
commitFormatString = Just "%FT%X%EZ"

defaultTimestamp :: Time
defaultTimestamp = Time "1970-01-01T00:00:00+00:00"

nobody :: Text
nobody = "ghost"

-- Generic utility fonction

fromIntToInt32 :: Int -> Int32
fromIntToInt32 = fromInteger . toInteger

removeSpace :: Text -> Text
removeSpace = TE.replace " " ""

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
getChangedFile DiffStats {..} = ChangedFile (fromIntToInt32 additions) (fromIntToInt32 deletions) (toLazy path)

getChangeNumber :: Text -> Int32
getChangeNumber iid =
  fromIntToInt32 $ fromMaybe 0 ((readMaybe $ toString iid) :: Maybe Int)

getChangeId :: Text -> Text -> LText
getChangeId fullName iid = toLazy . removeSpace $ TE.replace "/" "@" fullName <> "@" <> toText iid

toCommit :: MRCommit -> Commit
toCommit MRCommit {..} =
  Commit
    (toLazy sha)
    (Just . toIdent $ getAuthor cauthor)
    (Just . toIdent $ getAuthor cauthor)
    (Just . timeToTimestamp commitFormatString $ fromMaybe defaultTimestamp authoredDate)
    (Just . timeToTimestamp commitFormatString $ fromMaybe defaultTimestamp authoredDate)
    0
    0
    (toLazy $ fromMaybe "" ctitle)
  where
    getAuthor :: Maybe MRUserCore -> Text
    getAuthor (Just MRUserCore {..}) = username
    getAuthor Nothing = nobody

toIdent :: Text -> Ident
toIdent username = Ident (toLazy $ "gitlab.com" <> "/" <> username) (toLazy username)

ghostIdent :: Ident
ghostIdent = toIdent nobody

diffTime :: UTCTime -> UTCTime -> Int
diffTime l e =
  trunc $
    fromMaybe (error "Unable to read show nominalDiffTimeToSeconds") $
      readMaybe
        ( show $
            nominalDiffTimeToSeconds . negate $ diffUTCTime l e
        )
  where
    trunc :: Float -> Int
    trunc = truncate

toState :: Text -> Enumerated Change_ChangeState
toState state' = case state' of
  "closed" -> Enumerated (Right Change_ChangeStateClosed)
  "merged" -> Enumerated $ Right Change_ChangeStateMerged
  "opened" -> Enumerated $ Right Change_ChangeStateOpen
  _otherwise -> error ("Unable to decode Merge Request state: " <> _otherwise)

isMerged :: Enumerated Change_ChangeState -> Bool
isMerged state' = case state' of
  Enumerated (Right Change_ChangeStateMerged) -> True
  _otherwise -> False

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
