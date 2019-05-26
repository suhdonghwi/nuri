module Nuri.Parse where

import           Control.Monad

import           Data.Void
import           Data.Text                      ( Text )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

lineComment = L.skipLineComment "#"
blockComment = L.skipBlockComment "(*" "*)"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

returnKeywords :: Parser Text
returnKeywords = foldr1 (<|>) $ symbol <$> keywords
  where keywords = ["반환하다", "돌려주다"]


