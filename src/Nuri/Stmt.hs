module Nuri.Stmt where

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.Expr
import           Nuri.ASTNode

data Stmt = -- 표현식 구문 : 표현식
              ExprStmt Expr
            -- 반환 구문 : 표현식
            | Return Expr
            -- 대입 구문 : 코드 위치, 대입 대상 식별자, 대입하는 식
            | Assign SourcePos Text Expr
            -- 조건 분기 구문 : 코드 위치, 조건식, True시 실행 구문 집합, False시 실행 구문 집합
            | If SourcePos Expr Stmts (Maybe Stmts)
            -- 조건 반복 구문 : 조건식, 반복할 구문 집합
            | While SourcePos Expr Stmts
            -- 함수 선언 구문 : 코드 위치, 함수 이름, 함수 인자 이름, 함수 내부 구문 집합
            | FuncDecl SourcePos Text [Text] Stmts
          deriving (Show)

type Stmts = NonEmpty Stmt

instance Eq Stmt where
  ExprStmt e1    == ExprStmt e2    = e1 == e2
  Return   e1    == Return   e2    = e1 == e2
  Assign _ t1 e1 == Assign _ t2 e2 = (t1 == t2) && (e1 == e2)
  If _ e1 a1 b1  == If _ e2 a2 b2  = (e1 == e2) && (a1 == a2) && (b1 == b2)
  While _ e1 s1  == While _ e2 s2  = (e1 == e2) && (s1 == s2)
  FuncDecl _ f1 a1 b1 == FuncDecl _ f2 a2 b2 =
    (f1 == f2) && (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Stmt where
  srcPos (ExprStmt expr     ) = srcPos expr
  srcPos (Return   expr     ) = srcPos expr
  srcPos (Assign pos _ _    ) = pos
  srcPos (If pos _ _ _      ) = pos
  srcPos (While pos _ _     ) = pos
  srcPos (FuncDecl pos _ _ _) = pos
