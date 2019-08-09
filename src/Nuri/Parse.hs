module Nuri.Parse where

import           Control.Monad

import           Data.Void
import           Data.Text                                ( Text )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
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

reserved :: Text -> Parser ()
reserved s = (lexeme . try) (string s *> notFollowedBy hangulSyllable)

hangulSyllable :: Parser Char
hangulSyllable = oneOf ['가' .. '힣'] <?> "한글"

hangulJamo :: Parser Char
hangulJamo = oneOf (['ㄱ' .. 'ㅎ'] ++ ['ㅏ' .. 'ㅣ']) <?> "한글"
