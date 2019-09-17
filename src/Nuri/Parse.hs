module Nuri.Parse where

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                          ( (<?>) )
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

scn :: Parser ()
scn = L.space P.space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ P.takeWhile1P Nothing f) lineComment blockComment
  where f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved s = (lexeme . P.try) (P.string s *> P.notFollowedBy hangulSyllable)

hangulSyllable :: Parser Char
hangulSyllable = P.oneOf ['가' .. '힣'] <?> "한글"

hangulJamo :: Parser Char
hangulJamo = P.oneOf (['ㄱ' .. 'ㅎ'] ++ ['ㅏ' .. 'ㅣ']) <?> "한글"
