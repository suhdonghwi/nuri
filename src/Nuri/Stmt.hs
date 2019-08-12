module Nuri.Stmt where

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.Expr                                ( Expr )
import           Nuri.ASTNode                             ( ASTNode
                                                          , srcPos
                                                          )

data Stmt = Seq (NonEmpty Stmt)
          | ExprStmt Expr
          | Return Expr
          | If SourcePos Expr Stmt (Maybe Stmt)
          | FuncDecl SourcePos Text [Text] Stmt
          deriving (Show)

instance Eq Stmt where
  Seq      s1   == Seq      s2   = s1 == s2
  ExprStmt e1   == ExprStmt e2   = e1 == e2
  Return   e1   == Return   e2   = e1 == e2
  If _ e1 a1 b1 == If _ e2 a2 b2 = (e1 == e2) && (a1 == a2) && (b1 == b2)
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Stmt where
  srcPos (Seq      (stmt :| _)) = srcPos stmt
  srcPos (ExprStmt expr       ) = srcPos expr
  srcPos (Return   expr       ) = srcPos expr
  srcPos (If       pos _ _ _  ) = pos
  srcPos (FuncDecl pos _ _ _  ) = pos
