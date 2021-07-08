-- | Monocle search language parser
-- The goal of this module is to transform a list of 'token'
-- into a 'Expr'.
module Monocle.Search.Parser (parse) where

import qualified Control.Monad.Combinators as Combinators
import qualified Data.Set as Set
import qualified Data.Text as T
import Monocle.Search.Lexer (Token (..), lex)
import qualified Monocle.Search.Lexer as L
import Monocle.Search.Syntax (Expr (..), ParseError (..), SortOrder (..))
import Relude
import qualified Text.Megaparsec as Megaparsec

-- $setup
-- >>> let toLocated = map (\v -> L.LocatedToken 0 v 0)

-- | Short-hand type synonym used by parsing utilities
type Parser = Megaparsec.Parsec Void [L.LocatedToken]

-- | 'exprParser' parses an expression
--
-- >>> Megaparsec.parse exprParser "" (toLocated [L.Literal "status", L.Equal, L.Literal "open"])
-- Right (EqExpr "status" "open")
exprParser :: Parser Expr
exprParser = Combinators.choice [Megaparsec.try boolExpr, closedExpr]
  where
    -- 'boolExpr' combines multiple expression
    boolExpr = do
      -- Here we would like to run 'exprParser' directly, but that will cause
      -- a left recursion, so we are using a left factoring technique.
      leftExpr <- closedExpr
      operatorToken <- fromMaybe And <$> Combinators.optional (tokens [And, Or])
      case operatorToken of
        -- For the right expression, it is safe to run 'exprParser'
        And -> AndExpr leftExpr <$> exprParser
        Or -> OrExpr leftExpr <$> exprParser
        x -> error $ "this should not happen, see the expression before the case statement: " <> show x

    -- 'closedExpr' is a single expression
    closedExpr = Combinators.choice [notExpr, fieldExpr, parenExpr]

    -- 'notExpr' is a negated expression, we run 'closedExpr' so that 'boolExpr' keeps the priority.
    -- in other words, the 'notExpr' only applies to the next 'closedExpr'.
    notExpr = token Not >> NotExpr <$> closedExpr

    fieldExpr = do
      field <- literal
      operator <- do
        operatorToken <- tokens [Equal, Greater, Lower, GreaterEqual, LowerEqual]
        pure $ case operatorToken of
          Equal -> EqExpr
          Greater -> GtExpr
          Lower -> LtExpr
          GreaterEqual -> GtEqExpr
          LowerEqual -> LtEqExpr
          x -> error $ "this should not happen, see the expression before the case statement: " <> show x
      operator field <$> literal

    parenExpr =
      -- Here it is safe to run 'exprParser' because 'parenExpr' first parses an 'OpenParenthesis'
      Combinators.between (token OpenParenthesis) (token CloseParenthesis) exprParser

exprParserWithMods :: Parser (Maybe Expr)
exprParserWithMods = do
  expr <- Combinators.optional exprParser
  modifiers <- Combinators.many modExpr
  pure $ foldr (\modifier acc -> Just $ modifier acc) expr modifiers
  where
    -- 'modExpr' is a trailing expression modifiers
    modExpr = do
      operatorToken <- tokens [OrderBy, Limit]
      case operatorToken of
        OrderBy -> OrderByExpr <$> literal <*> orderSort
        Limit -> LimitExpr <$> intLiteral
        x -> error $ "this should not happen, see the expression before the case statement: " <> show x

    orderSort = do
      sortToken <- Combinators.optional $ tokens [SortAsc, SortDesc]
      pure $ case fromMaybe SortAsc sortToken of
        SortAsc -> Asc
        SortDesc -> Desc
        x -> error $ "this should not happen, see the expression before the case statement: " <> show x

-- | 'intLiteral' parses a number
intLiteral :: Parser Int
intLiteral = do
  strLiteral <- toString <$> literal
  case readMaybe strLiteral of
    Just int -> pure int
    Nothing -> Megaparsec.failure Nothing Set.empty

-- | 'literal' parses a literal token, returning it's value
literal :: Parser Text
literal = Megaparsec.token isLiteral Set.empty
  where
    isLiteral (L.LocatedToken _ (Literal v) _) = Just v
    isLiteral _ = Nothing

-- | 'token' parses a single token
token :: Token -> Parser Token
token token' = Megaparsec.token isToken Set.empty
  where
    isToken (L.LocatedToken _ v _)
      | v == token' = Just token'
      | otherwise = Nothing

-- | 'tokens' parses one of the tokens
tokens :: [Token] -> Parser Token
tokens = Combinators.choice . map token

-- | 'parse' parses the code into an 'Expr'
parse :: Text -> Either ParseError (Maybe Expr)
parse code = do
  tokens' <- lex code
  case Megaparsec.parse (exprParserWithMods <* Megaparsec.eof) "<input>" tokens' of
    Left err -> Left (mkErr err)
    Right expr -> Right expr
  where
    mkErr :: Megaparsec.ParseErrorBundle [L.LocatedToken] Void -> ParseError
    mkErr (Megaparsec.ParseErrorBundle (be :| _) _) =
      let (offset, message) = case be of
            Megaparsec.TrivialError _ (Just (Megaparsec.Tokens (L.LocatedToken start _ _end :| _))) _ -> (start, "Unexpected token")
            Megaparsec.TrivialError _ (Just Megaparsec.EndOfInput) _ -> (T.length code, "Unexpected end of input")
            x -> error $ "Unknown parsec error: " <> show x
       in ParseError ("Invalid syntax: " <> message) offset
