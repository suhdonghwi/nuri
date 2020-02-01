module Nuri.Parse.Stmt where

import           Control.Monad.Combinators.NonEmpty       ( some )

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt


parseStmts :: Parser (NonEmpty Stmt)
parseStmts = some (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt = DeclStmt <$> parseDecl
