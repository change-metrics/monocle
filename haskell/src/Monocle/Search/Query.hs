-- | Monocle search language query
-- The goal of this module is to transform a 'Expr' into a 'Bloodhound.Query'
module Monocle.Search.Query
  ( Query (..),
    queryWithMods,
    query,
    queryBH,
    ensureMinBound,
    fields,
    loadAliases,
    loadAliases',
    load,

    -- *
    QueryFlavor (..),
    AuthorFlavor (..),
    RangeFlavor (..),
    rangeField,
    defaultQueryFlavor,
    toBHQuery,
    toBHQueryWithFlavor,
    dropDate,
    dropField,
  )
where

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Char (isDigit)
import Data.List (lookup)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import Monocle.Search (Field_Type (..))
import qualified Monocle.Search.Parser as P
import Monocle.Search.Syntax

-- | Handle author filter:
-- The event author of a comment event is the comment author.
-- The change author is the author of a change.
-- Change event also has on author attribute, which is the change author,
-- e.g. the one who received event.
--
-- In other words, to get the amount of review received, use 'OnAuthor'
data AuthorFlavor = Author | OnAuthor deriving (Show, Eq)

-- | Handle date filter:
-- Change document has both createdAt and updatedAt.
-- Event document only have createdAt.
--
-- In other words, to get all the change updated recently, use UpdatedAt.
-- But to get all the event (such as review event), use CreatedAt.
data RangeFlavor = OnCreatedAt | CreatedAt | UpdatedAt deriving (Show, Eq)

rangeField :: RangeFlavor -> Text
rangeField = \case
  OnCreatedAt -> "on_created_at"
  CreatedAt -> "created_at"
  UpdatedAt -> "updated_at"

data QueryFlavor = QueryFlavor
  { qfAuthor :: AuthorFlavor,
    qfRange :: RangeFlavor
  }
  deriving (Show, Eq)

defaultQueryFlavor :: QueryFlavor
defaultQueryFlavor = QueryFlavor Author CreatedAt

-- | Deprecated accessor to get the bloodhound query from the query
{-# DEPRECATED queryBH "Use queryGet instead" #-}
queryBH :: Query -> QueryFlavor -> [BH.Query]
queryBH q qf = queryGet q id qf

data Query = Query
  { -- | queryGet provide the bloodhound query.
    -- it is a list to be combined as a bool query
    queryGet :: (Maybe Expr -> Maybe Expr) -> QueryFlavor -> [BH.Query],
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

-- instance Show Query where
--  show Query {..} = "Query {" <> show queryBounds <> "}"

toBHQuery :: Query -> [BH.Query]
toBHQuery = flip queryBH defaultQueryFlavor

toBHQueryWithFlavor :: QueryFlavor -> Query -> [BH.Query]
toBHQueryWithFlavor = flip queryBH

-- $setup
-- >>> import Monocle.Search.Parser as P
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.Time.Clock (getCurrentTime)
-- >>> now <- getCurrentTime

type Bound = (Maybe UTCTime, UTCTime)

data Env = Env
  { envNow :: UTCTime,
    envUsername :: Text,
    envIndex :: Config.Index,
    envFlavor :: QueryFlavor
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

-- | A fake field to ensure a field name is resolved using the flavor
flavoredField :: Field
flavoredField = error "Field name should be set at runtime"

-- | 'fields' specifies how to handle field value
fields :: [(Field, (FieldType, Field, Text))]
fields =
  [ ("updated_at", (fieldDate, "updated_at", "Last update")),
    ("created_at", (fieldDate, "created_at", "Change creation")),
    ("from", (fieldDate, flavoredField, "Range starting date")),
    ("to", (fieldDate, flavoredField, "Range ending date")),
    ("state", (fieldText, "state", "Change state, one of: open, merged, self_merged, abandoned")),
    ("repo", (fieldText, "repository_fullname", "Repository name")),
    ("repo_regex", (fieldRegex, "repository_fullname", "Repository regex")),
    ("project", (fieldText, "project_def", "Project definition name")),
    ("author", (fieldText, flavoredField, "Author name")),
    ("author_regex", (fieldRegex, flavoredField, "Author regex")),
    ("group", (fieldText, flavoredField, "Group definition name")),
    ("branch", (fieldText, "target_branch", "Branch name")),
    ("approval", (fieldText, "approval", "Approval name")),
    ("priority", (fieldText, "tasks_data.priority", "Task priority")),
    ("severity", (fieldText, "tasks_data.severity", "Task severity")),
    ("task", (fieldText, "tasks_data.ttype", "Task type")),
    ("score", (fieldNumber, "tasks_data.score", "PM score"))
  ]

-- | Resolves the actual document field for a given flavor
getFlavoredField :: QueryFlavor -> Field -> Maybe Field
getFlavoredField QueryFlavor {..} field
  | field `elem` ["author", "author_regex", "group"] = Just $ case qfAuthor of
    Author -> "author"
    OnAuthor -> "on_author"
  | field `elem` ["from", "to"] = Just $ rangeField qfRange
  | otherwise = Nothing

-- | 'lookupField' return a field type and actual field name
lookupField :: Field -> Parser (FieldType, Field, Text)
lookupField name = case lookup name fields of
  Just (fieldType, field, desc) -> do
    flavor <- asks envFlavor
    pure (fieldType, fromMaybe field $ getFlavoredField flavor name, desc)
  Nothing -> toParseError (Left $ "Unknown field: " <> name)

parseDateValue :: Text -> Maybe UTCTime
parseDateValue txt = tryParse "%F" <|> tryParse "%Y-%m" <|> tryParse "%Y"
  where
    tryParse fmt = parseTimeM False defaultTimeLocale fmt (toString txt)

subUTCTimeSecond :: UTCTime -> Integer -> UTCTime
subUTCTimeSecond date sec =
  addUTCTime (secondsToNominalDiffTime (fromInteger sec * (-1))) date

parseRelativeDateValue :: UTCTime -> Text -> Maybe UTCTime
parseRelativeDateValue now txt
  | "now" == txt = Just now
  | Text.isPrefixOf "now-" txt = tryParseRange (Text.drop 4 txt)
  | otherwise = Nothing
  where
    tryParseRange :: Text -> Maybe UTCTime
    tryParseRange txt' = do
      let countTxt = Text.takeWhile isDigit txt'
          valTxt = Text.dropWhileEnd (== 's') $ Text.drop (Text.length countTxt) txt'
          hour = 3600
          day = hour * 24
          week = day * 7
      count <- readMaybe (toString countTxt)
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
  _anyOtherExpr -> error "Unsupported range expression"

-- | dropTime ensures the encoded date does not have millisecond.
-- This actually discard hour differences
dropTime :: UTCTime -> UTCTime
dropTime (UTCTime day _sec) = UTCTime day 0

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
    Field_TypeFIELD_DATE | field `notElem` ["from", "to"] -> do
      date <-
        dropTime
          <$> ( toParseError
                  . note ("Invalid date: " <> value)
                  $ parseRelativeDateValue now value <|> parseDateValue value
              )

      updateBound op date

      pure $ toRangeValueD op date
    Field_TypeFIELD_NUMBER -> toParseError $ toRangeValue op <$> parseNumber value
    _anyOtherField -> toParseError . Left $ "Field " <> field <> " does not support range operator"

mkRangeQuery' :: RangeOp -> Field -> FieldType -> Text -> Parser BH.Query
mkRangeQuery' op field fieldType value =
  BH.QueryRangeQuery
    . BH.mkRangeQuery (BH.FieldName field)
    <$> mkRangeValue op field fieldType value

mkRangeAlias :: RangeOp -> Field -> Text -> Parser BH.Query
mkRangeAlias op field = mkRangeQuery' op field fieldDate

toParseError :: Either Text a -> Parser a
toParseError e = case e of
  Left msg -> lift . lift $ throwE (ParseError msg 0)
  Right x -> pure x

mkRangeQuery :: Expr -> Field -> Text -> Parser BH.Query
mkRangeQuery expr field value = do
  (fieldType, fieldName, _desc) <- lookupField field
  mkRangeQuery' (toRangeOp expr) fieldName fieldType value

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

-- | Resolve the author field name and value.
getAuthorField :: Field -> Text -> Parser (Field, Text)
getAuthorField fieldName = \case
  "self" -> do
    index <- asks envIndex
    username <- asks envUsername
    when (username == mempty) (toParseError $ Left "You need to be logged in to use the self value")
    pure $ case Config.lookupIdent index username of
      Just muid -> (fieldName <> ".muid", muid)
      Nothing -> (fieldName <> ".id", username)
  value -> pure $ (fieldName <> ".muid", value)

mkEqQuery :: Field -> Text -> Parser BH.Query
mkEqQuery field value' = do
  (fieldType, fieldName', _desc) <- lookupField field
  (fieldName, value) <-
    if fieldName' `elem` ["author", "on_author"]
      then getAuthorField fieldName' value'
      else pure (fieldName', value')
  case (field, fieldType) of
    ("from", _) -> mkRangeAlias Gt fieldName value
    ("to", _) -> mkRangeAlias Lt fieldName value
    ("state", _) -> do
      (stateField, stateValue) <-
        toParseError
          ( case value of
              "open" -> Right ("state", "OPEN")
              "merged" -> Right ("state", "MERGED")
              "self_merged" -> Right ("self_merged", "true")
              "abandoned" -> Right ("state", "CLOSED")
              _ -> Left $ "Invalid value for state: " <> value
          )
      pure $ BH.TermQuery (BH.Term stateField stateValue) Nothing
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
      pure $ BH.TermsQuery fieldName groupMembers
    (_, Field_TypeFIELD_BOOL) -> toParseError $ flip BH.TermQuery Nothing . BH.Term fieldName <$> parseBoolean value
    (_, Field_TypeFIELD_REGEX) ->
      pure
        . BH.QueryRegexpQuery
        $ BH.RegexpQuery (BH.FieldName fieldName) (BH.Regexp value) BH.AllRegexpFlags Nothing
    _anyOtherField -> pure $ BH.TermQuery (BH.Term fieldName value) Nothing

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
--  let query = load Nothing mempty Nothing "state:open"
--   in putTextLn . decodeUtf8 . Aeson.encode $ (queryBH query defaultQueryFlavor)
-- :}
-- [{"term":{"state":{"value":"OPEN"}}}]
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

queryWithMods :: UTCTime -> Text -> Maybe Config.Index -> Maybe Expr -> Either ParseError Query
queryWithMods now' username indexM exprM = do
  -- Compute whenever bound are provided
  (queryBounds, queryMinBoundsSet) <- case exprM of
    Nothing -> pure ((threeWeeksAgo now, now), False)
    Just expr -> do
      (_, (minBoundM, maxBound')) <- runParser expr defaultQueryFlavor
      pure ((fromMaybe (threeWeeksAgo maxBound') minBoundM, maxBound'), isJust minBoundM)

  -- Prepare the queryGet accessor
  let queryGet modifier flavor = case modifier exprM of
        Just expr' ->
          -- We have a query Expr, convert it to a BH.query
          let queryBHE = runParser expr' flavor
           in case queryBHE of
                Left e -> error $ "Could not convert the expr to a bloodhound query: " <> show e
                Right (queryFlavored, _) -> [queryFlavored]
        Nothing -> []
  pure $ Query {..}
  where
    runParser expr flavor =
      runExcept
        . flip runStateT (Nothing, now)
        . runReaderT (query expr)
        $ Env now username index flavor
    now = dropTime now'
    index = fromMaybe (error "need index") indexM
    threeWeeksAgo date = subUTCTimeSecond date (3600 * 24 * 7 * 3)

-- | Utility function to simply create a query
load :: Maybe UTCTime -> Text -> Maybe Config.Index -> Text -> Query
load nowM username indexM code = case P.parse [] code >>= queryWithMods now username indexM of
  Right x -> x
  Left err -> error (show err)
  where
    now = fromMaybe (error "need time") nowM

loadAliases' :: Config.Index -> [(Text, Expr)]
loadAliases' = fromRight (error "Alias loading failed") . loadAliases

loadAliases :: Config.Index -> Either [Text] [(Text, Expr)]
loadAliases index = case partitionEithers $ map loadAlias (Config.getAliases index) of
  ([], xs) -> Right xs
  (xs, _) -> Left xs
  where
    fakeNow :: UTCTime
    fakeNow = fromMaybe (error "not utctime?") $ readMaybe "2021-06-02 23:00:00 Z"
    loadAlias :: (Text, Text) -> Either Text (Text, Expr)
    loadAlias (name, code) = do
      let toError :: Either ParseError a -> Either Text a
          toError = \case
            -- TODO: improve error reporting
            Left e -> Left $ "Invalid alias " <> name <> ": " <> show e
            Right x -> Right x

      exprM <- toError $ P.parse [] code

      -- Try to evaluate the alias with fake value
      _testQuery <-
        toError $
          queryWithMods fakeNow "self" (Just index) exprM

      case exprM of
        Just expr ->
          -- We now know the alias can be converted to a bloodhound query
          Right (name, expr)
        Nothing -> Left $ "Empty alias " <> name

-- | Ensure a minimum range bound is set
-- TODO: re-implement as a `QueryM a -> QueryM a` combinator
ensureMinBound :: Query -> Query
ensureMinBound query'
  | queryMinBoundsSet query' = query'
  | otherwise = query' {queryGet = newQueryGet}
  where
    newQueryGet modifier flavor = queryGet query' (newModifier modifier) flavor
    -- A modifier function that ensure a min bound is set, whenever the user provided an expr.
    newModifier modifier exprM = case exprM of
      Just expr -> modifier $ Just $ AndExpr minExpr expr
      Nothing -> modifier $ Just $ minExpr
    minExpr = GtExpr "from" $ toText $ formatTime defaultTimeLocale "%F" (fst $ queryBounds query')

-- | dropField remove a field from an Expr
--
-- >>> let testDF = dropField (== "date") . Just
-- >>> testDF $ AndExpr (EqExpr "f" "v") (EqExpr "g" "v")
-- Just (AndExpr (EqExpr "f" "v") (EqExpr "g" "v"))
--
-- >>> dropField (== "date") $ Just $ AndExpr (EqExpr "date" "v") (EqExpr "g" "v")
-- Just (EqExpr "g" "v")
--
-- >>> dropField (== "date") $ Just $ AndExpr (EqExpr "f" "v") (EqExpr "date" "v")
-- Just (EqExpr "f" "v")
--
-- >>> dropField (== "date") $ Just $ AndExpr (EqExpr "date" "v") (EqExpr "date" "v")
-- Nothing
-- >>> dropField (== "date") $ Just $ EqExpr "date" "v"
-- Nothing
dropField :: (Text -> Bool) -> Maybe Expr -> Maybe Expr
dropField _ Nothing = Nothing
dropField dropFieldPred (Just expr) = go expr
  where
    go e = case e of
      AndExpr e1 e2 -> tryBoth AndExpr e1 e2 <|> go e1 <|> go e2
      OrExpr e1 e2 -> tryBoth OrExpr e1 e2 <|> go e1 <|> go e2
      NotExpr e1 -> Just . NotExpr =<< go e1
      EqExpr field _ -> unlessField field e
      GtExpr field _ -> unlessField field e
      LtExpr field _ -> unlessField field e
      GtEqExpr field _ -> unlessField field e
      LtEqExpr field _ -> unlessField field e
    tryBoth op e1 e2 = do
      e1' <- go e1
      e2' <- go e2
      pure $ op e1' e2'
    unlessField field e
      | dropFieldPred field = Nothing
      | otherwise = Just e

dropDate :: Maybe Expr -> Maybe Expr
dropDate = dropField (`elem` ["from", "to", "updated_at", "created_at"])
