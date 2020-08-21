module Nuri.Parse.Stmt where

import Control.Monad.Combinators.NonEmpty (some)
import Nuri.Parse (Parser, sc, scn)
import Nuri.Parse.Decl (parseDecl)
import Nuri.Parse.Expr (parseExpr)
import Nuri.Stmt (Stmt (..))
import Text.Megaparsec.Char.Lexer (nonIndented)

parseStmts :: Parser (NonEmpty Stmt)
parseStmts = scn >> some (nonIndented sc parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt = parseDeclStmt <|> parseExprStmt

parseDeclStmt :: Parser Stmt
parseDeclStmt = DeclStmt <$> parseDecl parseExpr

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> parseExpr
