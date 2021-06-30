{-# LANGUAGE NoImplicitPrelude #-}

-- | The Monocle Search Language Syntax
module Monocle.Search.Syntax
  ( Expr (..),
    SortOrder (..),
    ParseError (..),
    Query (..),
    toBHQuery,
    setQueryBH,
  )
where

import Data.Time.Clock (UTCTime)
import qualified Database.Bloodhound as BH
import Relude

type Field = Text

type Value = Text

data SortOrder = Asc | Desc deriving (Show, Eq)

data Expr
  = AndExpr Expr Expr
  | OrExpr Expr Expr
  | NotExpr Expr
  | -- Field operator
    EqExpr Field Value
  | GtExpr Field Value
  | LtExpr Field Value
  | GtEqExpr Field Value
  | LtEqExpr Field Value
  | -- Search operator
    OrderByExpr Field SortOrder (Maybe Expr)
  | LimitExpr Int (Maybe Expr)
  deriving (Show, Eq)

data ParseError = ParseError Text Int
  deriving (Show, Eq)

data Query = Query
  { queryOrder :: Maybe (Text, SortOrder),
    queryLimit :: Int,
    queryBH :: Maybe BH.Query,
    -- | queryBounds is the (minimum, maximum) date found anywhere in the query.
    -- It defaults to (now-3weeks, now)
    -- It doesn't prevent empty bounds, e.g. `date>2021 and date<2020` results in (2021, 2020).
    -- It doesn't check the fields, e.g. `created_at>2020 and updated_at<2021` resuls in (2020, 2021).
    -- It keeps the maximum minbound and minimum maxbound, e.g.
    --  `date>2020 and date>2021` results in (2021, now).
    -- The goal is to get an approximate bound for histo grams queries.
    queryBounds :: (UTCTime, UTCTime),
    -- | queryBoundsSet indicate when a minimum bound has been set by the user.
    queryMinBoundsSet :: Bool
  }
  deriving (Show)

toBHQuery :: Query -> [BH.Query]
toBHQuery = maybeToList . queryBH

setQueryBH :: BH.Query -> Query -> Query
setQueryBH queryBH' query = query {queryBH = Just queryBH'}
