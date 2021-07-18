-- | The Monocle Search Language Syntax
module Monocle.Search.Syntax
  ( Expr (..),
    ParseError (..),
    Query (..),
    QueryFlavor (..),
    AuthorFlavor (..),
    RangeFlavor (..),
    rangeField,
    defaultQueryFlavor,
    toBHQuery,
    toBHQueryWithFlavor,
  )
where

import Data.Time.Clock (UTCTime)
import qualified Database.Bloodhound as BH
import Relude hiding (show)
import Prelude (show)

type Field = Text

type Value = Text

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
  deriving (Show, Eq)

data ParseError = ParseError Text Int
  deriving (Show, Eq)

data AuthorFlavor = Author | OnAuthor deriving (Show, Eq)

data RangeFlavor = CreatedAt | UpdatedAt deriving (Show, Eq)

rangeField :: RangeFlavor -> Text
rangeField = \case
  CreatedAt -> "created_at"
  UpdatedAt -> "updated_at"

data QueryFlavor = QueryFlavor
  { qfAuthor :: AuthorFlavor,
    qfRange :: RangeFlavor
  }
  deriving (Show, Eq)

defaultQueryFlavor :: QueryFlavor
defaultQueryFlavor = QueryFlavor Author CreatedAt

data Query = Query
  { -- | queryBH is the bloodhound query
    -- it is a list to be combine as a bool query
    queryBH :: QueryFlavor -> [BH.Query],
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

instance Show Query where
  show Query {..} = "Query {" <> show queryBounds <> "}"

toBHQuery :: Query -> [BH.Query]
toBHQuery = flip queryBH defaultQueryFlavor

toBHQueryWithFlavor :: QueryFlavor -> Query -> [BH.Query]
toBHQueryWithFlavor = flip queryBH
