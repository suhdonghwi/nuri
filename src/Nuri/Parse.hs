module Nuri.Parse where

import Nuri.Parse.PartTable (MonadPartTable)
import Text.Megaparsec (MonadParsec, (<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type MonadParser m = (MonadParsec Void Text m, MonadFail m, MonadPartTable m)

lineComment :: (MonadParser m) => m ()
lineComment = L.skipLineComment "#"

blockComment :: (MonadParser m) => m ()
blockComment = L.skipBlockComment "(*" "*)"

-- Space Consumer, 공백 문자 (개행 문자 제외) 를 스킵할 때 쓰입니다.
sc :: (MonadParser m) => m ()
sc = L.space (void $ P.takeWhile1P Nothing isSpace) lineComment blockComment
  where
    isSpace x = x == ' ' || x == '\t'

-- Space Consumer with Newline, 개행 문자를 포함한 공백 문자를 스킵할 때 쓰입니다.
scn :: (MonadParser m) => m ()
scn = L.space P.space1 lineComment blockComment

lexeme :: (MonadParser m) => m a -> m a
lexeme = L.lexeme sc

symbol :: (MonadParser m) => Text -> m Text
symbol = L.symbol sc

reserved :: (MonadParser m) => String -> m ()
reserved s =
  (lexeme . P.try) (sequence (P.char <$> toString s) *> P.notFollowedBy hangulSyllable)
    <?> concat ["'", s, "'"]

hangulSyllable :: (MonadParser m) => m Char
hangulSyllable = P.hidden $ P.satisfy (\x -> '가' <= x && x <= '힣')

hangulJamo :: (MonadParser m) => m Char
hangulJamo =
  P.hidden $ P.satisfy (\x -> ('ㄱ' <= x && x <= 'ㅎ') || ('ㅏ' <= x && x <= 'ㅣ'))
