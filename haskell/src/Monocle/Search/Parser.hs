{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language parser
-- The goal of this module is to transform a list of 'token'
-- into a 'Expr'.
module Monocle.Search.Parser (parse) where

import qualified Control.Monad.Combinators as Combinators
import qualified Data.Set as Set
import Monocle.Search.Lexer (Token (..), lex)
import Monocle.Search.Syntax (Expr (..))
import Relude
import qualified Text.Megaparsec as Megaparsec

-- | Short-hand type synonym used by parsing utilities
type Parser = Megaparsec.Parsec Void [Token]

-- | 'exprParser' parses an expression
--
-- >>> Megaparsec.parse exprParser "" [Literal "status", Equal, Literal "open"]
-- Right (EqExpr "status" "open")
exprParser :: Parser Expr
exprParser = Combinators.choice [Megaparsec.try boolExpr, searchExpr]
  where
    -- 'boolExpr' combines multiple expression
    boolExpr = do
      -- Here we would like to run 'exprParser' directly, but that will cause
      -- a left recursion, so we are using a left factoring technique.
      leftExpr <- closedExpr
      operatorToken <- tokens [And, Or]
      case operatorToken of
        -- For the right expression, it is safe to run 'exprParser'
        And -> AndExpr leftExpr <$> exprParser
        Or -> OrExpr leftExpr <$> exprParser
        _ -> error "this should not happen, see the expression before the case statement"

    -- 'searchExpr' is a single expression (e.g. `a:b`) with optional modifiers (e.g. `order by x`)
    searchExpr = do
      expr <- closedExpr
      modifiers <- Combinators.many modExpr
      pure $ foldr (\modifier acc -> modifier acc) expr modifiers

    -- 'closedExpr' is a single expression without modifiers
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
          _ -> error "this should not happen, see the expression before the case statement"
      operator field <$> literal

    parenExpr =
      -- Here it is safe to run 'exprParser' because 'parenExpr' first parses an 'OpenParenthesis'
      Combinators.between (token OpenParenthesis) (token CloseParenthesis) exprParser

    -- 'modExpr' is a trailing expression modifiers
    modExpr = do
      operatorToken <- tokens [OrderBy, Limit]
      case operatorToken of
        OrderBy -> OrderByExpr <$> literal
        Limit -> LimitExpr <$> intLiteral
        _ -> error "this should not happen, see the expression before the case statement"

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
    isLiteral (Literal v) = Just v
    isLiteral _ = Nothing

-- | 'token' parses a single token
token :: Token -> Parser Token
token token' = Megaparsec.token isToken Set.empty
  where
    isToken v
      | v == token' = Just token'
      | otherwise = Nothing

-- | 'tokens' parses one of the tokens
tokens :: [Token] -> Parser Token
tokens = Combinators.choice . map token

-- | 'parse' parses the code into an 'Expr'
parse :: Text -> Either Text Expr
parse code = do
  tokens' <- lex code
  case Megaparsec.parse (exprParser <* Megaparsec.eof) "<input>" tokens' of
    Left err -> Left $ show err
    Right expr -> Right expr
