module Nuri.Parse.Stmt where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

indent :: Parser (L.IndentOpt Parser a b) -> Parser a
indent = L.indentBlock scn

parseStmts :: Parser Stmts
parseStmts = fromList <$> P.some (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt = parseFuncDecl

parseFuncDecl :: Parser Stmt
parseFuncDecl = do
  pos <- getSourceLine
  P.try $ reserved "함수"
  args     <- P.many parseIdentifier
  funcName <- parseFuncIdentifier
  _        <- symbol ":"
  FuncDecl pos funcName args <$> parseExpr
