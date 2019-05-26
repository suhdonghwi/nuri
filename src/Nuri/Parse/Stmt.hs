module Nuri.Parse.Stmt where

import           Text.Megaparsec

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr

returnStmt :: Parser Stmt
returnStmt = Return <$> (expr <* returnKeywords)
