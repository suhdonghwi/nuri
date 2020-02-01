module Nuri.Stmt where

import           Nuri.ASTNode
import           Nuri.Decl
import           Nuri.Expr

data Stmt = DeclStmt Decl
          | ExprStmt Expr
  deriving (Eq, Show)

instance ASTNode Stmt where
  getSourceLine (DeclStmt decl) = getSourceLine decl
  getSourceLine (ExprStmt expr) = getSourceLine expr
