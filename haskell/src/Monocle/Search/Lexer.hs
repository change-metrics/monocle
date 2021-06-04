{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle search language lexer
-- The goal of this module is to transform a 'Text'
-- into a list of 'Token'.
module Monocle.Search.Lexer (Token (..), LocatedToken (..), lex) where

import qualified Control.Monad.Combinators as Combinators
import Monocle.Search.Syntax (ParseError (..))
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

data LocatedToken = LocatedToken {start :: Int, token :: Token, end :: Int}
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

locatedTokenParser :: Parser LocatedToken
locatedTokenParser = LocatedToken <$> Megaparsec.getOffset <*> tokenParser <*> Megaparsec.getOffset

-- | 'literal' parses a literal field or value
literal :: Parser Text
literal = Lexer.lexeme Megaparsec.Char.space $ Combinators.choice [direct, quoted]
  where
    direct = Megaparsec.takeWhile1P Nothing isValueChar
    quoted = "\"" *> Megaparsec.takeWhile1P Nothing isText <* "\""
    isValueChar c =
      ('\x23' <= c && c <= '\x25')
        || c == '\x27'
        || ('\x2A' <= c && c <= '\x39')
        || c == '\x3B'
        || ('\x3F' <= c && c <= '\x7B')
        || ('\x7D' <= c && c <= '\x10FFFF')
    isText c = ('\x20' <= c && c <= '\x21') || ('\x23' <= c && c <= '\x10FFFF')

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
tokensParser :: Parser [LocatedToken]
tokensParser = Megaparsec.Char.space *> many locatedTokenParser <* Megaparsec.eof

-- | 'lex' parses the code into a list of 'Token'
lex :: Text -> Either ParseError [LocatedToken]
lex code = case Megaparsec.parse tokensParser "<input>" code of
  Left err -> Left (mkErr err)
  Right tokens -> Right tokens
  where
    mkErr (Megaparsec.ParseErrorBundle (be :| _) _) =
      let offset = case be of
            Megaparsec.TrivialError x _ _ -> x
            Megaparsec.FancyError x _ -> x
       in ParseError "Invalid token" offset
