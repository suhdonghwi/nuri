module Nuri.Parse where

import           Data.Void
import           Data.Text                      ( Text )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

returnKeywords :: Parser Text
returnKeywords = foldr1 (<|>) $ symbol <$> keywords
  where keywords = ["반환하다", "돌려주다"]


