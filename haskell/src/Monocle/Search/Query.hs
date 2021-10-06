-- | Monocle search language query
-- The goal of this module is to transform a 'Expr' into a 'Bloodhound.Query'
module Monocle.Search.Query
  ( Query (..),
    queryWithMods,
    query,
    ensureMinBound,
    fields,
    queryFieldToDocument,
    loadAliases,
    loadAliases',
    load,

    -- *
    QueryFlavor (..),
    AuthorFlavor (..),
    RangeFlavor (..),
    rangeField,
    defaultQueryFlavor,
    dropDate,
    dropField,
    blankQuery,
    yearAgo,
  )
where

import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.List (lookup)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Database.Bloodhound as BH
import qualified Monocle.Api.Config as Config
import Monocle.Prelude hiding (parseDateValue)
import Monocle.Search (Field_Type (..))
import qualified Monocle.Search.Parser as P
import Monocle.Search.Syntax
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (lift)
import Text.Read (readPrec)
import qualified Text.Read.Lex (readDecP)

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
data RangeFlavor = OnCreatedAt | CreatedAt | UpdatedAt | OnCreatedAndCreated deriving (Show, Eq)

rangeField :: RangeFlavor -> Maybe Text
rangeField = \case
  OnCreatedAt -> Just "on_created_at"
  CreatedAt -> Just "created_at"
  UpdatedAt -> Just "updated_at"
  -- OnCreatedAndCreated flavor does not match a single field
  OnCreatedAndCreated -> Nothing

data QueryFlavor = QueryFlavor
  { qfAuthor :: AuthorFlavor,
    qfRange :: RangeFlavor
  }
  deriving (Show, Eq)

defaultQueryFlavor :: QueryFlavor
defaultQueryFlavor = QueryFlavor Author CreatedAt

blankQuery :: UTCTime -> UTCTime -> Query
blankQuery since to =
  let queryGet _ _ = []
      queryMinBoundsSet = True
      queryBounds = (dropTime to, dropTime since)
   in Query {..}

yearAgo :: UTCTime -> UTCTime
yearAgo since = subUTCTimeSecond since (3600 * 24 * 365)

data Query = Query
  { -- | queryGet provide the bloodhound query.
    -- it is a list to be combined as a bool query
    queryGet :: (Maybe Expr -> Maybe Expr) -> Maybe QueryFlavor -> [BH.Query],
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

-- $setup
-- >>> import Monocle.Search.Parser as P
-- >>> import qualified Data.Aeson as Aeson
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
    ("repo", (fieldRegex, "repository_fullname", "Repository name")),
    ("project", (fieldText, "project_def", "Project definition name")),
    ("author", (fieldRegex, flavoredField, "Author name")),
    ("group", (fieldText, flavoredField, "Group definition name")),
    ("branch", (fieldRegex, "target_branch", "Branch name")),
    ("approval", (fieldText, "approval", "Approval name")),
    ("priority", (fieldText, "tasks_data.priority", "Task priority")),
    ("severity", (fieldText, "tasks_data.severity", "Task severity")),
    ("tag", (fieldRegex, "tasks_data.ttype", "Task type")),
    ("score", (fieldNumber, "tasks_data.score", "PM score"))
  ]

queryFieldToDocument :: Field -> Maybe Field
queryFieldToDocument name = do
  (_, field, _) <- lookup name fields
  pure field

-- | Resolves the actual document field for a given flavor
getFlavoredField :: QueryFlavor -> Field -> Maybe Field
getFlavoredField QueryFlavor {..} field
  | field `elem` ["author", "group"] = Just $ case qfAuthor of
    Author -> "author"
    OnAuthor -> "on_author"
  | field `elem` ["from", "to"] = rangeField qfRange
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

data TimeRange = Hour | Day | Week
  deriving (Show)

timeRangeReader :: ReadP.ReadP TimeRange
timeRangeReader = (hourR <|> dayR <|> weekR) <* ReadP.optional (ReadP.char 's')
  where
    hourR = Hour <$ ReadP.string "hour"
    dayR = Day <$ ReadP.string "day"
    weekR = Week <$ ReadP.string "week"

instance Read TimeRange where
  readPrec = ReadPrec.lift timeRangeReader

data RelativeTime = MkRelativeTime Word TimeRange
  deriving (Show)

relTimeReader :: ReadP.ReadP RelativeTime
relTimeReader = ReadP.string "now" *> (relR <|> pure defTime)
  where
    defTime = MkRelativeTime 0 Hour
    relR = ReadP.char '-' *> (MkRelativeTime <$> countR <*> timeRangeReader)
    countR :: ReadP.ReadP Word
    countR = Text.Read.Lex.readDecP

instance Read RelativeTime where
  readPrec = ReadPrec.lift relTimeReader

parseRelativeDateValue :: UTCTime -> Text -> Maybe UTCTime
parseRelativeDateValue now txt = relTimeToUTCTime <$> readMaybe (toString txt)
  where
    relTimeToUTCTime :: RelativeTime -> UTCTime
    relTimeToUTCTime (MkRelativeTime count range) =
      let hour = 3600
          day = hour * 24
          diffsec =
            (fromInteger . toInteger $ count) * case range of
              Hour -> hour
              Day -> day
              Week -> day * 7
       in subUTCTimeSecond now diffsec

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
    Field_TypeFIELD_DATE -> do
      date <-
        dropTime
          <$> ( toParseError
                  . note ("Invalid date: " <> value)
                  $ parseRelativeDateValue now value <|> parseDateValue value
              )

      when (date < oldestDate) (throwParseError $ "Date is too old: " <> show date)

      updateBound op date

      pure $ toRangeValueD op date
    Field_TypeFIELD_NUMBER -> toParseError $ toRangeValue op <$> parseNumber value
    _anyOtherField -> toParseError . Left $ "Field " <> field <> " does not support range operator"

oldestDate :: UTCTime
oldestDate = fromMaybe (error "oops") (readMaybe "1970-01-01 00:00:00 UTC")

mkRangeQuery :: RangeOp -> Field -> Text -> Parser BH.Query
mkRangeQuery op field value = do
  (fieldType, fieldName, _desc) <- lookupField field
  rangeValue <- mkRangeValue op field fieldType value

  let mkQuery fieldName' =
        BH.QueryRangeQuery $
          BH.mkRangeQuery (BH.FieldName fieldName') rangeValue

  QueryFlavor _ rf <- asks envFlavor
  pure $ case rf of
    OnCreatedAndCreated ->
      mkAnd [mkQuery "created_at", mkQuery "on_created_at"]
    _ -> mkQuery fieldName

throwParseError :: Text -> Parser a
throwParseError msg = lift . lift $ throwE (ParseError msg 0)

toParseError :: Either Text a -> Parser a
toParseError e = case e of
  Left msg -> throwParseError msg
  Right x -> pure x

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
    file = mkRegexpQ "changed_files.path"

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
    -- Handle custom field name that needs extra processing
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
    (_, Field_TypeFIELD_DATE) ->
      toParseError $ Left $ "Invalid date operator for: " <> field <> ", ':' is not allowed"
    _anyOtherField -> pure $ BH.TermQuery (BH.Term fieldName value) Nothing

data BoolOp = And | Or

-- | utility function to flatten boolean expression, so that:
--    Or (Or e1 e2) e3  => [e1, e2, e3]
takeWhileExpr :: BoolOp -> Expr -> [Expr]
takeWhileExpr op expr = case (op, expr) of
  (Or, OrExpr e1 e2) -> takeWhileExpr op e1 <> takeWhileExpr op e2
  (And, AndExpr e1 e2) -> takeWhileExpr op e1 <> takeWhileExpr op e2
  _anyOtherCase -> [expr]

mkBoolQuery :: BoolOp -> [Expr] -> Parser BH.Query
mkBoolQuery op es = do
  qs <- traverse query es
  let (must, should) = case op of
        And -> (qs, [])
        Or -> ([], qs)
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery must [] [] should

mkNotQuery :: Expr -> Parser BH.Query
mkNotQuery e1 = do
  q1 <- query e1
  pure $ BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [q1] []

-- | 'query' creates an elastic search query
query :: Expr -> Parser BH.Query
query expr = case expr of
  AndExpr {} -> mkBoolQuery And (takeWhileExpr And expr)
  OrExpr {} -> mkBoolQuery Or (takeWhileExpr Or expr)
  EqExpr field value
    | field == "from" -> mkRangeQuery Gt "from" value
    | field == "to" -> mkRangeQuery Lt "from" value
    | otherwise -> mkEqQuery field value
  NotExpr e1 -> mkNotQuery e1
  GtExpr field value -> mkRangeQuery Gt field value
  GtEqExpr field value -> mkRangeQuery Gte field value
  LtExpr field value -> mkRangeQuery Lt field value
  LtEqExpr field value -> mkRangeQuery Lte field value

queryWithMods :: UTCTime -> Text -> Config.Index -> Maybe Expr -> Either ParseError Query
queryWithMods now' username index exprM = do
  -- Compute whenever bound are provided
  (queryBounds, queryMinBoundsSet) <- case exprM of
    Nothing -> pure ((threeWeeksAgo now, now), False)
    Just expr -> do
      (_, (minBoundM, maxBound')) <- runParser expr defaultQueryFlavor
      pure ((fromMaybe (threeWeeksAgo maxBound') minBoundM, maxBound'), isJust minBoundM)

  -- Prepare the queryGet accessor
  let queryGet modifier flavorM = case modifier exprM of
        Just expr' ->
          -- We have a query Expr, convert it to a BH.query
          let queryBHE = runParser expr' (fromMaybe defaultQueryFlavor flavorM)
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
    threeWeeksAgo date = subUTCTimeSecond date (3600 * 24 * 7 * 3)

-- | Utility function to simply create a query
load :: UTCTime -> Text -> Config.Index -> Text -> Query
load now username index code = case P.parse [] code >>= queryWithMods now username index of
  Right x -> x
  Left err -> error (show err)

loadAliases' :: Config.Index -> [(Text, Expr)]
loadAliases' = fromRight (error "Alias loading failed") . loadAliases

loadAliases :: Config.Index -> Either [Text] [(Text, Expr)]
loadAliases index = case partitionEithers $ map loadAlias (Config.getAliases index) of
  ([], xs) -> Right xs
  (xs, _) -> Left xs
  where
    fakeNow :: UTCTime
    fakeNow = [utctime|2021-06-02 23:00:00|]
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
          queryWithMods fakeNow "self" index exprM

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
