{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language query
-- The goal of this module is to transform a 'Expr' into a 'Bloodhound.Query'
module Monocle.Search.Query (Query (..), queryWithMods, query, fields) where

import Data.List (lookup)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Database.Bloodhound as BH
import Monocle.Search (Field_Type (..))
import Monocle.Search.Syntax (Expr (..), ParseError (..), SortOrder (..))
import Relude

type Field = Text

type FieldType = Field_Type

fieldDate, fieldNumber, fieldText :: FieldType
fieldDate = Field_TypeFIELD_DATE
fieldNumber = Field_TypeFIELD_NUMBER
fieldText = Field_TypeFIELD_TEXT

-- | 'fields' specifies how to handle field value
fields :: [(Field, (FieldType, Field, Text))]
fields =
  [ ("updated_at", (fieldDate, "updated_at", "Last update")),
    ("score", (fieldNumber, "tasks_data.score", "PM score")),
    ("repo", (fieldText, "repository_fullname", "Repository name")),
    ("author", (fieldText, "author.muid", "Author name"))
  ]

-- | 'lookupField' return a field type and actual field name
lookupField :: Field -> Either Text (FieldType, Field, Text)
lookupField name = maybe (Left $ "Unknown field: " <> name) Right (lookup name fields)

parseDateValue :: Text -> Either Text UTCTime
parseDateValue txt = case parseTimeM False defaultTimeLocale "%F" (toString txt) of
  Just value -> pure value
  Nothing -> Left $ "Invalid date: " <> txt

parseNumber :: Text -> Either Text Double
parseNumber txt = case readMaybe (toString txt) of
  Just value -> pure value
  Nothing -> Left $ "Invalid number: " <> txt

data RangeOp = Gt | Gte | Lt | Lte

toRangeOp :: Expr -> RangeOp
toRangeOp expr = case expr of
  GtExpr _ _ -> Gt
  LtExpr _ _ -> Lt
  GtEqExpr _ _ -> Gte
  LtEqExpr _ _ -> Lte
  _ -> error "Unsupported range expression"

toRangeValueD :: RangeOp -> (UTCTime -> BH.RangeValue)
toRangeValueD op = case op of
  Gt -> BH.RangeDateGt . BH.GreaterThanD
  Gte -> BH.RangeDateGte . BH.GreaterThanEqD
  Lt -> BH.RangeDateLt . BH.LessThanD
  Lte -> BH.RangeDateLte . BH.LessThanEqD

toRangeValue :: RangeOp -> (Double -> BH.RangeValue)
toRangeValue op = case op of
  Gt -> BH.RangeDoubleGt . BH.GreaterThan
  Gte -> BH.RangeDoubleGte . BH.GreaterThanEq
  Lt -> BH.RangeDoubleLt . BH.LessThan
  Lte -> BH.RangeDoubleLte . BH.LessThanEq

mkRangeValue :: RangeOp -> Field -> FieldType -> Text -> Either Text BH.RangeValue
mkRangeValue op field fieldType value = do
  case fieldType of
    Field_TypeFIELD_DATE -> toRangeValueD op <$> parseDateValue value
    Field_TypeFIELD_NUMBER -> toRangeValue op <$> parseNumber value
    _ -> Left $ "Field " <> field <> " does not support range operator"

toParseError :: Either Text a -> Either ParseError a
toParseError e = case e of
  Left msg -> Left (ParseError msg 0)
  Right x -> Right x

mkRangeQuery :: Expr -> Field -> Text -> Either ParseError BH.Query
mkRangeQuery expr field value = do
  (fieldType, fieldName, _desc) <- toParseError $ lookupField field
  BH.QueryRangeQuery
    . BH.mkRangeQuery (BH.FieldName fieldName)
    <$> (toParseError $ mkRangeValue (toRangeOp expr) field fieldType value)

mkEqQuery :: Field -> Text -> Either ParseError BH.Query
mkEqQuery field value = do
  (_fieldType, fieldName, _desc) <- toParseError $ lookupField field
  Right $ BH.TermQuery (BH.Term fieldName value) Nothing

data BoolOp = And | Or

mkBoolQuery :: BoolOp -> Expr -> Expr -> Either ParseError BH.Query
mkBoolQuery op e1 e2 = do
  q1 <- query e1
  q2 <- query e2
  let (must, should) = case op of
        And -> ([q1, q2], [])
        Or -> ([], [q1, q2])
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery must [] [] should

mkNotQuery :: Expr -> Either ParseError BH.Query
mkNotQuery e1 = do
  q1 <- query e1
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [q1] []

-- | 'query' creates an elastic search query
--
-- >>> let Right q = (Parser.parse "state:open" >>= query) in putTextLn . decodeUtf8 . Aeson.encode $ q
-- {"term":{"state":{"value":"open"}}}
query :: Expr -> Either ParseError BH.Query
query expr = case expr of
  AndExpr e1 e2 -> mkBoolQuery And e1 e2
  OrExpr e1 e2 -> mkBoolQuery Or e1 e2
  EqExpr field value -> mkEqQuery field value
  NotExpr e1 -> mkNotQuery e1
  e@(GtExpr field value) -> mkRangeQuery e field value
  e@(GtEqExpr field value) -> mkRangeQuery e field value
  e@(LtExpr field value) -> mkRangeQuery e field value
  e@(LtEqExpr field value) -> mkRangeQuery e field value
  LimitExpr _ _ -> Left (ParseError "Limit must be global" 0)
  OrderByExpr _ _ _ -> Left (ParseError "Order by must be global" 0)

data Query = Query {queryOrder :: Maybe (Field, SortOrder), queryLimit :: Int, queryBH :: BH.Query} deriving (Show)

queryWithMods :: Expr -> Either ParseError Query
queryWithMods baseExpr = Query order limit <$> query expr
  where
    (order, limit, expr) = case baseExpr of
      OrderByExpr order' sortOrder (LimitExpr limit' expr') -> (Just (order', sortOrder), limit', expr')
      LimitExpr limit' expr' -> (Nothing, limit', expr')
      _ -> (Nothing, 100, baseExpr)
