module Nuri.Stmt where

import           Text.Megaparsec.Pos

import           Nuri.Expr

data Stmt = ExprStmt Expr
          | Return Expr
          | Block [Stmt]
    deriving (Eq, Show)
