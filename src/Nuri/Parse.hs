module Nuri.Parse where

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                          ( (<?>) )
import           Text.Megaparsec.Pos                      ( Pos )

import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- Space Consumer, 공백 문자 (개행 문자 제외) 를 스킵할 때 쓰입니다.
sc :: Parser ()
sc = L.space (void $ P.takeWhile1P Nothing isSpace) lineComment blockComment
  where isSpace x = x == ' ' || x == '\t'

-- Space Consumer with Newline, 개행 문자를 포함한 공백 문자를 스킵할 때 쓰입니다.
scn :: Parser ()
scn = L.space P.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved s =
  (lexeme . P.try)
      (sequence (P.char <$> toString s) *> P.notFollowedBy hangulSyllable)
    <?> concat ["'", s, "'"]

hangulSyllable :: Parser Char
hangulSyllable = P.hidden $ P.satisfy (\x -> '가' <= x && x <= '힣')

hangulJamo :: Parser Char
hangulJamo =
  P.hidden $ P.satisfy (\x -> ('ㄱ' <= x && x <= 'ㅎ') || ('ㅏ' <= x && x <= 'ㅣ'))

getSourceLine :: Parser Pos
getSourceLine = P.sourceLine <$> P.getSourcePos

seqBlock :: Parser () -> Parser (L.IndentOpt Parser a b) -> Parser a
seqBlock spc r = do
  spc
  ref <- L.indentLevel
  a   <- r
  case a of
    L.IndentNone x          -> x <$ spc
    L.IndentMany indent f p -> do
      mlvl <- (optional . P.try) (P.eol *> L.indentLevel)
      done <- isJust <$> optional P.eof
      case (mlvl, done) of
        (Just lvl, False) ->
          indentedItems ref (fromMaybe lvl indent) spc p >>= f
        _ -> spc *> f []
    L.IndentSome indent f p -> do
      pos <- P.eol *> L.indentLevel
      let lvl = fromMaybe pos indent
      x <- if
        | pos <= ref -> L.incorrectIndent GT ref pos
        | pos == lvl -> p
        | otherwise  -> L.incorrectIndent EQ lvl pos
      xs <- indentedItems ref lvl spc p
      f (x : xs)
 where
  indentedItems :: Pos -> Pos -> Parser () -> Parser b -> Parser [b]
  indentedItems ref lvl spc p = go
   where
    go = do
      spc
      pos  <- L.indentLevel
      done <- isJust <$> optional P.eof
      if done
        then return []
        else if
          | pos <= ref -> return []
          | pos == lvl -> (:) <$> p <*> go
          | otherwise  -> L.incorrectIndent EQ lvl pos
