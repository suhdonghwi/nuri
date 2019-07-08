module Nuri.Parse.Stmt where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

stmt :: Parser Stmt
stmt = try returnStmt <|> try functionDecl <|> exprStmt

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> (expr <* notFollowedBy returnKeyword)

returnStmt :: Parser Stmt
returnStmt = Return <$> (expr <* returnKeyword)

functionDecl :: Parser Stmt
functionDecl = L.nonIndented scn (L.indentBlock scn p)
 where
  p = do
    pos  <- getSourcePos
    args <- many (char '[' *> identifier <* char ']')
    sc
    funcName <- funcIdentifier
    _        <- symbol ":"
    return (L.IndentSome Nothing (return . FuncDecl pos funcName args) stmt)
