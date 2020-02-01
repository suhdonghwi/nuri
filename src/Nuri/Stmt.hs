module Nuri.Stmt where

import           Nuri.ASTNode
import           Nuri.Decl

data Stmt = DeclStmt Decl
  deriving (Eq, Show)

instance ASTNode Stmt where
  getSourceLine (DeclStmt decl) = getSourceLine decl
