module Nuri.Stmt where

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.Expr
import           Nuri.ASTNode


data Stmt = ExprStmt Expr
          | Return Expr
          | If SourcePos Expr Stmts (Maybe Stmts)
          | While Expr Stmts
          | FuncDecl SourcePos Text [Text] Stmts
          deriving (Show)

type Stmts = NonEmpty Stmt

instance Eq Stmt where
  ExprStmt e1   == ExprStmt e2   = e1 == e2
  Return   e1   == Return   e2   = e1 == e2
  If _ e1 a1 b1 == If _ e2 a2 b2 = (e1 == e2) && (a1 == a2) && (b1 == b2)
  While e1 s1   == While e2 s2   = (e1 == e2) && (s1 == s2)
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Stmt where
  srcPos (ExprStmt expr     ) = srcPos expr
  srcPos (Return   expr     ) = srcPos expr
  srcPos (If pos _ _ _      ) = pos
  srcPos (While expr _      ) = srcPos expr
  srcPos (FuncDecl pos _ _ _) = pos
