{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language query
-- The goal of this module is to transform a 'Expr' into a 'Bloodhound.Query'
module Monocle.Search.Query (Query (..), queryWithMods, query, fields, load) where

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Char (isDigit)
import Data.List (lookup)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (Field_Type (..))
import qualified Monocle.Search.Parser as P
import Monocle.Search.Syntax (Expr (..), ParseError (..), SortOrder (..))

-- $setup
-- >>> import Monocle.Search.Parser as P
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.Time.Clock (getCurrentTime)
-- >>> now <- getCurrentTime

type Bound = (Maybe UTCTime, UTCTime)

data Env = Env
  { envNow :: UTCTime,
    envUsername :: Text,
    envIndex :: Config.Index
  }

type Parser a = ReaderT Env (StateT Bound (Except ParseError)) a

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
    ("project", (fieldText, "project_def", "Project definition name")),
    ("author", (fieldText, "author.muid", "Author name")),
    ("author_regex", (fieldRegex, "author.muid", "Author regex")),
    ("group", (fieldText, "group_def", "Group definition name")),
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

subUTCTimeSecond :: UTCTime -> Integer -> UTCTime
subUTCTimeSecond date sec =
  addUTCTime (secondsToNominalDiffTime (fromInteger sec * (-1))) date

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
      pure $ subUTCTimeSecond now diffsec

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

isMinOp :: RangeOp -> Bool
isMinOp op = case op of
  Gt -> True
  Gte -> True
  Lt -> False
  Lte -> False

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

updateBound :: RangeOp -> UTCTime -> Parser ()
updateBound op date = do
  (minDateM, maxDate) <- get
  put $ newBounds minDateM maxDate
  where
    newBounds minDateM maxDate =
      if isMinOp op
        then (Just $ max date (fromMaybe date minDateM), maxDate)
        else (minDateM, min date maxDate)

mkRangeValue :: RangeOp -> Field -> FieldType -> Text -> Parser BH.RangeValue
mkRangeValue op field fieldType value = do
  now <- asks envNow
  case fieldType of
    Field_TypeFIELD_DATE -> do
      date <-
        toParseError
          . note ("Invalid date: " <> value)
          $ parseRelativeDateValue now value <|> parseDateValue value

      updateBound op date

      pure $ toRangeValueD op date
    Field_TypeFIELD_NUMBER -> toParseError $ toRangeValue op <$> parseNumber value
    _ -> toParseError . Left $ "Field " <> field <> " does not support range operator"

toParseError :: Either Text a -> Parser a
toParseError e = case e of
  Left msg -> lift . lift $ throwE (ParseError msg 0)
  Right x -> pure x

mkRangeQuery :: Expr -> Field -> Text -> Parser BH.Query
mkRangeQuery expr field value = do
  (fieldType, fieldName, _desc) <- toParseError $ lookupField field
  BH.QueryRangeQuery
    . BH.mkRangeQuery (BH.FieldName fieldName)
    <$> mkRangeValue (toRangeOp expr) field fieldType value

mkProjectQuery :: Config.Project -> BH.Query
mkProjectQuery Config.Project {..} = BH.QueryBoolQuery $ BH.mkBoolQuery must [] [] []
  where
    must =
      map BH.QueryRegexpQuery $
        maybe [] repository repository_regex
          <> maybe [] branch branch_regex
          <> maybe [] file file_regex
    mkRegexpQ field value =
      [BH.RegexpQuery (BH.FieldName field) (BH.Regexp value) BH.AllRegexpFlags Nothing]
    repository = mkRegexpQ "repository_fullname"
    branch = mkRegexpQ "target_branch"
    -- TODO: check how to regexp match nested list
    file = const [] -- mkRegexpQ "changed_files"

mkEqQuery :: Field -> Text -> Parser BH.Query
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
    ("author", _) -> do
      (field', value') <- case value of
        "self" -> do
          index <- asks envIndex
          username <- asks envUsername
          when (username == mempty) (toParseError $ Left "You need to be logged in to use the self value")
          pure $ case Config.lookupIdent index username of
            Just muid -> ("author.muid", muid)
            Nothing -> ("author.id", username)
        _ -> pure ("author.muid", value)
      pure $ BH.TermQuery (BH.Term field' value') Nothing
    ("project", _) -> do
      index <- asks envIndex
      project <-
        toParseError $
          Config.lookupProject index value `orDie` ("Unknown project: " <> value)
      pure $ mkProjectQuery project
    ("group", _) -> do
      index <- asks envIndex
      groupMembers <-
        toParseError $
          Config.lookupGroupMembers index value `orDie` ("Unknown group: " <> value)
      pure $ BH.TermsQuery "author.muid" groupMembers
    (_, Field_TypeFIELD_BOOL) -> toParseError $ flip BH.TermQuery Nothing . BH.Term fieldName <$> parseBoolean value
    (_, Field_TypeFIELD_REGEX) ->
      pure
        . BH.QueryRegexpQuery
        $ BH.RegexpQuery (BH.FieldName fieldName) (BH.Regexp value) BH.AllRegexpFlags Nothing
    _ -> pure $ BH.TermQuery (BH.Term fieldName value) Nothing

data BoolOp = And | Or

mkBoolQuery :: BoolOp -> Expr -> Expr -> Parser BH.Query
mkBoolQuery op e1 e2 = do
  q1 <- query e1
  q2 <- query e2
  let (must, should) = case op of
        And -> ([q1, q2], [])
        Or -> ([], [q1, q2])
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery must [] [] should

mkNotQuery :: Expr -> Parser BH.Query
mkNotQuery e1 = do
  q1 <- query e1
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [q1] []

-- | 'query' creates an elastic search query
--
-- >>> :{
--  let Right expr = P.parse "state:open"
--      Right (q, _) = runExcept $ runStateT (query now expr) (Nothing, now)
--   in putTextLn . decodeUtf8 . Aeson.encode $ q
-- :}
-- {"term":{"state":{"value":"OPEN"}}}
query :: Expr -> Parser BH.Query
query expr = case expr of
  AndExpr e1 e2 -> mkBoolQuery And e1 e2
  OrExpr e1 e2 -> mkBoolQuery Or e1 e2
  EqExpr field value -> mkEqQuery field value
  NotExpr e1 -> mkNotQuery e1
  e@(GtExpr field value) -> mkRangeQuery e field value
  e@(GtEqExpr field value) -> mkRangeQuery e field value
  e@(LtExpr field value) -> mkRangeQuery e field value
  e@(LtEqExpr field value) -> mkRangeQuery e field value
  LimitExpr {} -> lift . lift $ throwE (ParseError "Limit must be global" 0)
  OrderByExpr {} -> lift . lift $ throwE (ParseError "Order by must be global" 0)

data Query = Query
  { queryOrder :: Maybe (Field, SortOrder),
    queryLimit :: Int,
    queryBH :: Maybe BH.Query,
    -- | queryBounds is the (minimum, maximum) date found anywhere in the query.
    -- It defaults to (now-3weeks, now)
    -- It doesn't prevent empty bounds, e.g. `date>2021 and date<2020` results in (2021, 2020).
    -- It doesn't check the fields, e.g. `created_at>2020 and updated_at<2021` resuls in (2020, 2021).
    -- It keeps the maximum minbound and minimum maxbound, e.g.
    --  `date>2020 and date>2021` results in (2021, now).
    -- The goal is to get an approximate bound for histo grams queries.
    queryBounds :: (UTCTime, UTCTime)
  }
  deriving (Show)

queryWithMods :: UTCTime -> Text -> Maybe Config.Index -> Maybe Expr -> Either ParseError Query
queryWithMods now username indexM baseExprM =
  case exprM of
    Nothing -> pure $ Query order limit Nothing (threeWeeksAgo now, now)
    Just expr -> do
      (query', (boundM, bound)) <-
        runExcept
          . flip runStateT (Nothing, now)
          . runReaderT (query expr)
          $ Env now username index
      pure $
        Query order limit (Just query') (fromMaybe (threeWeeksAgo bound) boundM, bound)
  where
    index = fromMaybe (error "need index") indexM
    threeWeeksAgo date = subUTCTimeSecond date (3600 * 24 * 7 * 3)
    (order, limit, exprM) = case baseExprM of
      (Just (OrderByExpr order' sortOrder (Just (LimitExpr limit' expr')))) -> (Just (order', sortOrder), limit', expr')
      (Just (LimitExpr limit' (Just (OrderByExpr order' sortOrder expr')))) -> (Just (order', sortOrder), limit', expr')
      (Just (LimitExpr limit' expr')) -> (Nothing, limit', expr')
      expr' -> (Nothing, 100, expr')

-- | Utility function to simply create a query
load :: Maybe UTCTime -> Text -> Maybe Config.Index -> Text -> Query
load nowM username indexM code = case P.parse code >>= queryWithMods now username indexM of
  Right x -> x
  Left err -> error (show err)
  where
    now = fromMaybe (error "need time") nowM
