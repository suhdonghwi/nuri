module Nuri.Stmt where

import           Data.Text
import           Data.List.NonEmpty

import           Text.Megaparsec.Pos

import           Nuri.Expr
import           Nuri.ASTNode

data Stmt = ExprStmt Expr
          | Return Expr
          | If SourcePos (NonEmpty (Expr, [Stmt])) (Maybe [Stmt])
          | FuncDecl SourcePos Text [Text] [Stmt]
    deriving (Show)

instance Eq Stmt where
  ExprStmt e1 == ExprStmt e2 = e1 == e2
  Return   e1 == Return   e2 = e1 == e2
  If _ a1 b1  == If _ a2 b2  = (a1 == a2) && (b1 == b2)
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Stmt where
  srcPos (ExprStmt expr     ) = srcPos expr
  srcPos (Return   expr     ) = srcPos expr
  srcPos (If pos _ _        ) = pos
  srcPos (FuncDecl pos _ _ _) = pos
