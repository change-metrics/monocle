{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language lexer
-- The goal of this module is to transform a 'Text'
-- into a list of 'Token'.
module Monocle.Search.Lexer (Token (..), lex) where

import qualified Control.Monad.Combinators as Combinators
import Relude
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- | Short-hand type synonym used by lexing utilities
type Parser = Megaparsec.Parsec Void Text

-- | The language is composed of the following token:
data Token
  = Literal Text
  | -- boolean operator
    And
  | Or
  | Not
  | -- field operator
    Equal
  | Greater
  | GreaterEqual
  | Lower
  | LowerEqual
  | -- search operator
    OrderBy
  | SortAsc
  | SortDesc
  | Limit
  | -- piority operator
    OpenParenthesis
  | CloseParenthesis
  deriving (Show, Eq, Ord)

-- | 'tokenParser' parses a single token
--
-- >>> Megaparsec.parse tokenParser "" "and"
-- Right And
-- >>> Megaparsec.parse tokenParser "" "state"
-- Right (Literal "state")
tokenParser :: Parser Token
tokenParser =
  Combinators.choice
    [ And <$ keyword ["and", "AND"] ["∧", "&&"],
      Or <$ keyword ["or", "OR"] ["∨", "||"],
      Not <$ keyword ["not", "NOT"] ["¬", "!"],
      Equal <$ keyword [] [":", "=", "=="],
      OrderBy <$ keyword ["order by", "ORDER BY"] [],
      SortAsc <$ keyword ["asc", "ASC"] [],
      SortDesc <$ keyword ["desc", "DESC"] [],
      Limit <$ keyword ["limit", "LIMIT"] [],
      Greater <$ symbol ">",
      GreaterEqual <$ symbol ">=",
      Lower <$ symbol "<",
      LowerEqual <$ symbol "<=",
      OpenParenthesis <$ symbol "(",
      CloseParenthesis <$ symbol ")",
      Literal <$> literal
    ]

-- | 'literal' parses a literal field or value
literal :: Parser Text
literal = Lexer.lexeme Megaparsec.Char.space $ Megaparsec.takeWhile1P Nothing isLiteral
  where
    isLiteral x = upper x || lower x || digit x || sep x
    upper c = 'A' <= c && c <= 'Z'
    lower c = 'a' <= c && c <= 'z'
    digit c = '0' <= c && c <= '9'
    sep c = c `elem` ['-', '_', '/']

-- | 'symbol' parses a known symbol
symbol :: Text -> Parser Text
symbol = Lexer.symbol Megaparsec.Char.space

-- | 'text' parses a known text with a required trailing space
name :: Text -> Parser Text
name = Lexer.symbol $ Megaparsec.Char.space1 <|> Megaparsec.eof

-- | 'keyword' parses a list of known names or symbols
keyword :: [Text] -> [Text] -> Parser Text
keyword names symbols =
  Megaparsec.try $ Combinators.choice (map name names <|> map symbol symbols)

-- | 'tokenParser' parses all the token until the end of file
tokensParser :: Parser [Token]
tokensParser = Megaparsec.Char.space *> many tokenParser <* Megaparsec.eof

-- | 'lex' parses the code into a list of 'Token'
lex :: Text -> Either Text [Token]
lex code = case Megaparsec.parse tokensParser "<input>" code of
  Left err -> Left $ show err
  Right tokens -> Right tokens
