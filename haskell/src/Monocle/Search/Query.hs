{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language query
-- The goal of this module is to transform a 'Expr' into a 'Bloodhound.Query'
module Monocle.Search.Query (Query (..), queryWithMods, query, fields) where

import Data.Char (isDigit)
import Data.List (lookup)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Database.Bloodhound as BH
import Monocle.Search (Field_Type (..))
import Monocle.Search.Syntax (Expr (..), ParseError (..), SortOrder (..))
import Relude

-- $setup
-- >>> import Monocle.Search.Parser as P
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.Time.Clock (getCurrentTime)
-- >>> now <- getCurrentTime

type Field = Text

type FieldType = Field_Type

fieldDate, fieldNumber, fieldText {- fieldBoolean, -}, fieldRegex :: FieldType
fieldDate = Field_TypeFIELD_DATE
fieldNumber = Field_TypeFIELD_NUMBER
fieldText = Field_TypeFIELD_TEXT
-- fieldBoolean = Field_TypeFIELD_BOOL
fieldRegex = Field_TypeFIELD_REGEX

-- | 'fields' specifies how to handle field value
fields :: [(Field, (FieldType, Field, Text))]
fields =
  [ ("updated_at", (fieldDate, "updated_at", "Last update")),
    ("created_at", (fieldDate, "created_at", "Change creation")),
    ("state", (fieldText, "state", "Change state, one of: open, merged, self_merged, abandoned")),
    ("repo", (fieldText, "repository_fullname", "Repository name")),
    ("repo_regex", (fieldRegex, "repository_fullname", "Repository regex")),
    ("author", (fieldText, "author.muid", "Author name")),
    ("author_regex", (fieldRegex, "author.muid", "Author regex")),
    ("branch", (fieldText, "target_branch", "Branch name")),
    ("approval", (fieldText, "approval", "Approval name")),
    ("priority", (fieldText, "tasks_data.priority", "Task priority")),
    ("severity", (fieldText, "tasks_data.severity", "Task severity")),
    ("task", (fieldText, "tasks_data.ttype", "Task type")),
    ("score", (fieldNumber, "tasks_data.score", "PM score"))
  ]

-- | 'lookupField' return a field type and actual field name
lookupField :: Field -> Either Text (FieldType, Field, Text)
lookupField name = maybe (Left $ "Unknown field: " <> name) Right (lookup name fields)

parseDateValue :: Text -> Maybe UTCTime
parseDateValue txt = case tryParse "%F" <|> tryParse "%Y-%m" <|> tryParse "%Y" of
  Just value -> pure value
  Nothing -> Nothing
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt (toString txt)

parseRelativeDateValue :: UTCTime -> Text -> Maybe UTCTime
parseRelativeDateValue now txt
  | Text.isPrefixOf "now-" txt = tryParseRange (Text.drop 4 txt)
  | otherwise = Nothing
  where
    tryParseRange :: Text -> Maybe UTCTime
    tryParseRange txt' = do
      let countTxt = Text.takeWhile isDigit txt'
          count :: Integer
          count = fromMaybe (error $ "Invalid relative count: " <> txt') $ readMaybe (toString countTxt)
          valTxt = Text.dropWhileEnd (== 's') $ Text.drop (Text.length countTxt) txt'
          hour = 3600
          day = hour * 24
          week = day * 7
      diffsec <-
        (* count) <$> case valTxt of
          "hour" -> Just hour
          "day" -> Just day
          "week" -> Just week
          _ -> Nothing
      pure $ addUTCTime (secondsToNominalDiffTime (fromInteger diffsec) * (-1)) now

parseNumber :: Text -> Either Text Double
parseNumber txt = case readMaybe (toString txt) of
  Just value -> pure value
  Nothing -> Left $ "Invalid number: " <> txt

parseBoolean :: Text -> Either Text Text
parseBoolean txt = case txt of
  "true" -> pure "true"
  "false" -> pure "false"
  _ -> Left $ "Invalid booolean: " <> txt

data RangeOp = Gt | Gte | Lt | Lte

note :: Text -> Maybe a -> Either Text a
note err value = case value of
  Just a -> Right a
  Nothing -> Left err

toRangeOp :: Expr -> RangeOp
toRangeOp expr = case expr of
  GtExpr _ _ -> Gt
  LtExpr _ _ -> Lt
  GtEqExpr _ _ -> Gte
  LtEqExpr _ _ -> Lte
  _ -> error "Unsupported range expression"

-- | dropTime ensures the encoded date does not have millisecond.
-- This actually discard hour differences
dropTime :: UTCTime -> UTCTime
dropTime (UTCTime day _sec) = UTCTime day 0

toRangeValueD :: RangeOp -> (UTCTime -> BH.RangeValue)
toRangeValueD op = case op of
  Gt -> BH.RangeDateGt . BH.GreaterThanD . dropTime
  Gte -> BH.RangeDateGte . BH.GreaterThanEqD . dropTime
  Lt -> BH.RangeDateLt . BH.LessThanD . dropTime
  Lte -> BH.RangeDateLte . BH.LessThanEqD . dropTime

toRangeValue :: RangeOp -> (Double -> BH.RangeValue)
toRangeValue op = case op of
  Gt -> BH.RangeDoubleGt . BH.GreaterThan
  Gte -> BH.RangeDoubleGte . BH.GreaterThanEq
  Lt -> BH.RangeDoubleLt . BH.LessThan
  Lte -> BH.RangeDoubleLte . BH.LessThanEq

mkRangeValue :: UTCTime -> RangeOp -> Field -> FieldType -> Text -> Either Text BH.RangeValue
mkRangeValue now op field fieldType value = do
  case fieldType of
    Field_TypeFIELD_DATE ->
      toRangeValueD op
        <$> (note ("Invalid date: " <> value) $ parseRelativeDateValue now value <|> parseDateValue value)
    Field_TypeFIELD_NUMBER -> toRangeValue op <$> parseNumber value
    _ -> Left $ "Field " <> field <> " does not support range operator"

toParseError :: Either Text a -> Either ParseError a
toParseError e = case e of
  Left msg -> Left (ParseError msg 0)
  Right x -> Right x

mkRangeQuery :: UTCTime -> Expr -> Field -> Text -> Either ParseError BH.Query
mkRangeQuery now expr field value = do
  (fieldType, fieldName, _desc) <- toParseError $ lookupField field
  BH.QueryRangeQuery
    . BH.mkRangeQuery (BH.FieldName fieldName)
    <$> (toParseError $ mkRangeValue now (toRangeOp expr) field fieldType value)

mkEqQuery :: Field -> Text -> Either ParseError BH.Query
mkEqQuery field value = do
  (fieldType, fieldName, _desc) <- toParseError $ lookupField field
  case (field, fieldType) of
    ("state", _) -> do
      (field', value') <-
        toParseError
          ( case value of
              "open" -> Right ("state", "OPEN")
              "merged" -> Right ("state", "MERGED")
              "self_merged" -> Right ("self_merged", "true")
              "abandoned" -> Right ("state", "CLOSED")
              _ -> Left $ "Invalid value for state: " <> value
          )
      pure $ BH.TermQuery (BH.Term field' value') Nothing
    (_, Field_TypeFIELD_BOOL) -> toParseError $ flip BH.TermQuery Nothing . BH.Term fieldName <$> parseBoolean value
    (_, Field_TypeFIELD_REGEX) ->
      Right
        . BH.QueryRegexpQuery
        $ BH.RegexpQuery (BH.FieldName fieldName) (BH.Regexp value) BH.AllRegexpFlags Nothing
    _ -> Right $ BH.TermQuery (BH.Term fieldName value) Nothing

data BoolOp = And | Or

mkBoolQuery :: UTCTime -> BoolOp -> Expr -> Expr -> Either ParseError BH.Query
mkBoolQuery now op e1 e2 = do
  q1 <- query now e1
  q2 <- query now e2
  let (must, should) = case op of
        And -> ([q1, q2], [])
        Or -> ([], [q1, q2])
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery must [] [] should

mkNotQuery :: UTCTime -> Expr -> Either ParseError BH.Query
mkNotQuery now e1 = do
  q1 <- query now e1
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [q1] []

-- | 'query' creates an elastic search query
--
-- >>> :{
--  let Right q = P.parse "state:open" >>= query now
--   in putTextLn . decodeUtf8 . Aeson.encode $ q
-- :}
-- {"term":{"state":{"value":"OPEN"}}}
query :: UTCTime -> Expr -> Either ParseError BH.Query
query now expr = case expr of
  AndExpr e1 e2 -> mkBoolQuery now And e1 e2
  OrExpr e1 e2 -> mkBoolQuery now Or e1 e2
  EqExpr field value -> mkEqQuery field value
  NotExpr e1 -> mkNotQuery now e1
  e@(GtExpr field value) -> mkRangeQuery now e field value
  e@(GtEqExpr field value) -> mkRangeQuery now e field value
  e@(LtExpr field value) -> mkRangeQuery now e field value
  e@(LtEqExpr field value) -> mkRangeQuery now e field value
  LimitExpr _ _ -> Left (ParseError "Limit must be global" 0)
  OrderByExpr _ _ _ -> Left (ParseError "Order by must be global" 0)

data Query = Query {queryOrder :: Maybe (Field, SortOrder), queryLimit :: Int, queryBH :: BH.Query} deriving (Show)

queryWithMods :: UTCTime -> Expr -> Either ParseError Query
queryWithMods now baseExpr = Query order limit <$> query now expr
  where
    (order, limit, expr) = case baseExpr of
      OrderByExpr order' sortOrder (LimitExpr limit' expr') -> (Just (order', sortOrder), limit', expr')
      LimitExpr limit' expr' -> (Nothing, limit', expr')
      _ -> (Nothing, 100, baseExpr)
