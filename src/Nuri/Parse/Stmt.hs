module Nuri.Parse.Stmt where

import Control.Monad.Combinators.NonEmpty (some)
import Nuri.Parse ( sc, scn, Parser )
import Nuri.Parse.Expr ( parseDecl, parseExpr )
import Nuri.Stmt ( Stmt(..) )
import Text.Megaparsec.Char.Lexer (nonIndented)

parseStmts :: Parser (NonEmpty Stmt)
parseStmts = scn >> some (nonIndented sc parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt = parseDeclStmt <|> parseExprStmt

parseDeclStmt :: Parser Stmt
parseDeclStmt = DeclStmt <$> parseDecl

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> parseExpr
