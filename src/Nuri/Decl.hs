module Nuri.Decl where

import           Text.Megaparsec.Pos                      ( Pos )

import           Nuri.ASTNode
import           Nuri.Expr

data Decl = FuncDecl Pos String [String] Expr
          | ConstDecl Pos String Expr
  deriving (Show)

instance Eq Decl where
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  ConstDecl _ n1 e1 == ConstDecl _ n2 e2 = (n1 == n2) && (e1 == e2)
  _                 == _                 = False

instance ASTNode Decl where
  getSourceLine (FuncDecl pos _ _ _) = pos
  getSourceLine (ConstDecl pos _ _ ) = pos

declToExpr :: Decl -> (Pos, String, Expr)
declToExpr (FuncDecl pos funcName args body) =
  (pos, funcName, Lambda pos args body)
declToExpr (ConstDecl pos constName expr) = (pos, constName, expr)

declToLet :: Decl -> (Expr -> Expr)
declToLet decl = let (pos, name, expr) = declToExpr decl in Let pos name expr
