module Nuri.Decl where

import           Text.Megaparsec.Pos

import           Nuri.ASTNode
import           Nuri.Expr

data Decl = FuncDecl Pos String [String] Expr
  deriving (Show)

instance Eq Decl where
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Decl where
  getSourceLine (FuncDecl pos _ _ _) = pos
