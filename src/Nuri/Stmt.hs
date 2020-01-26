module Nuri.Stmt where

import           Text.Megaparsec.Pos                      ( Pos )

import           Nuri.ASTNode
import           Nuri.Expr

data Stmt = -- 함수 선언 구문 : 코드 위치, 함수 이름, 함수 인자 이름, 함수 내부 구문 집합
            FuncDecl Pos String [String] Expr
          deriving (Show)

type Stmts = [Stmt]

instance Eq Stmt where
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Stmt where
  getSourceLine (FuncDecl pos _ _ _) = pos
