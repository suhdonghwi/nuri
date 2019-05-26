module Nuri.Stmt where

import           Text.Megaparsec.Pos

import           Nuri.Expr
import           Nuri.ASTNode

data Stmt = ExprStmt Expr
          | Return Expr
          | Block [Stmt]
    deriving (Eq, Show)

instance ASTNode Stmt where
  srcPos (ExprStmt expr) = srcPos expr
  srcPos (Return expr) = srcPos expr
  srcPos (Block (expr : _)) = srcPos expr
