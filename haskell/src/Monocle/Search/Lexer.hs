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
    [ And <$ keywords ["and", "AND"],
      Or <$ keywords ["or", "OR"],
      Not <$ keywords ["not", "NOT"],
      Equal <$ op ":",
      Greater <$ op ">",
      GreaterEqual <$ op ">=",
      Lower <$ op "<",
      LowerEqual <$ op "<=",
      OpenParenthesis <$ keyword "(",
      CloseParenthesis <$ keyword ")",
      Literal <$> literal
    ]

locatedTokenParser :: Parser LocatedToken
locatedTokenParser = LocatedToken <$> Megaparsec.getOffset <*> tokenParser <*> Megaparsec.getOffset

-- | 'literal' parses a literal field or value
literal :: Parser Text
literal = Combinators.choice [direct, quoted] <* Megaparsec.Char.space
  where
    direct = Megaparsec.takeWhile1P Nothing isValueChar
    quoted = "\"" *> Megaparsec.takeWhile1P Nothing isText <* "\""
    isValueChar c =
      ('\x23' <= c && c <= '\x27')
        || ('\x2A' <= c && c <= '\x39')
        || c == '\x3B'
        || ('\x3F' <= c && c <= '\x10FFFF')
    isText c = ('\x20' <= c && c <= '\x21') || ('\x23' <= c && c <= '\x10FFFF')

-- | 'keywords' parses one of the keywords
keywords :: [Text] -> Parser Text
keywords = Combinators.choice . map keyword

-- | 'keyword' parses a keyword with optional space around
keyword :: Text -> Parser Text
keyword = Lexer.symbol Megaparsec.Char.space

-- | 'op' parses an operator without space arounds
op :: Text -> Parser Text
op = Megaparsec.chunk

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
