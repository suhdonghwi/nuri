module Nuri.Parse where

import Nuri.Expr (Decl (Decl), DeclKind (..), DeclType (FuncDecl))
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (Pos)

type Parser = P.ParsecT Void Text (State [Decl])

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- Space Consumer, 공백 문자 (개행 문자 제외) 를 스킵할 때 쓰입니다.
sc :: Parser ()
sc = L.space (void $ P.takeWhile1P Nothing isSpace) lineComment blockComment
  where
    isSpace x = x == ' ' || x == '\t'

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

resolveDecl :: Text -> [DeclKind] -> Int -> Parser DeclKind
resolveDecl name kinds offset = do
  st <- get
  case find (checkDecl name) st of
    Just (Decl _ _ (FuncDecl kind _ _)) -> return kind
    _ -> do
      let missingKinds = intercalate " 또는 " (declKindName <$> kinds)
      P.setOffset offset
      fail $ missingKinds ++ " '" ++ toString name ++ "'을(를) 찾을 수 없습니다."
  where
    checkDecl n' (Decl _ n _) = n == n'
    declKindName kind =
      case kind of
        NormalDecl -> "함수"
        VerbDecl -> "동사"
        AdjectiveDecl -> "형용사"