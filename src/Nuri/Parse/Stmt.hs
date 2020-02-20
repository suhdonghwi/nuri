module Nuri.Parse.Stmt where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

import           Control.Monad.Combinators.NonEmpty       ( some )

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

parseStmts :: Parser (NonEmpty Stmt)
parseStmts = some (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt = parseDeclStmt <|> parseExprStmt

parseDeclStmt :: Parser Stmt
parseDeclStmt = DeclStmt <$> parseDecl <* (void P.newline <|> void P.eof)

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> parseExpr
