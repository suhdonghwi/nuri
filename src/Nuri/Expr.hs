module Nuri.Expr where


import           Text.Megaparsec.Pos                      ( Pos )

import           Nuri.ASTNode
import           Nuri.Literal

data Expr = -- 리터럴 표현식 : 코드 위치, 리터럴 값
              Lit Pos Literal
            -- 변수 읽기 표현식 : 코드 위치, 변수명 
            | Var Pos String
            -- 함수 호출 표현식 : 코드 위치, 함수식, 함수 인자 리스트
            | FuncCall Pos Expr [(Expr, String)]
            -- 조건 분기 표현식 : 코드 위치, 조건식, then문, else문
            | If Pos Expr Expr Expr
            -- 이항 연산 표현식 : 코드 위치, 이항 연산자, 피연산자(좌), 피연산자(우)
            | BinaryOp Pos BinaryOperator Expr Expr
            -- 단항 연산 표현식 : 코드 위치, 단항 연산자, 피연산자
            | UnaryOp Pos UnaryOperator Expr
            -- 표현식 시퀀스 : 표현식 목록
            | Seq (NonEmpty Expr)
            -- 람다 표현식 : 코드 위치, 인수 목록, 함수 본문
            | Lambda Pos [(String, String)] Expr
            -- 상수 선언 표현식 : 코드 위치, 상수 이름, 상수 값, 표현식
            | Let Pos String Expr Expr
          deriving (Show)

instance Eq Expr where
  Lit _ v1         == Lit _ v2         = v1 == v2
  Var _ v1         == Var _ v2         = v1 == v2
  FuncCall _ f1 a1 == FuncCall _ f2 a2 = (f1 == f2) && (a1 == a2)
  If _ c1 t1 e1    == If _ c2 t2 e2    = (c1 == c2) && (t1 == t2) && (e1 == e2)
  BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 =
    (op1 == op2) && (l1 == l2) && (r1 == r2)
  UnaryOp _ op1 v1 == UnaryOp _ op2 v2 = (op1 == op2) && (v1 == v2)
  Seq e1           == Seq e2           = e1 == e2
  Lambda _ a1 b1   == Lambda _ a2 b2   = (a1 == a2) && (b1 == b2)
  Let _ n1 v1 e1   == Let _ n2 v2 e2   = (n1 == n2) && (v1 == v2) && (e1 == e2)
  _                == _                = False

instance ASTNode Expr where
  getSourceLine (Lit pos _         ) = pos
  getSourceLine (Var pos _         ) = pos
  getSourceLine (FuncCall pos _ _  ) = pos
  getSourceLine (BinaryOp pos _ _ _) = pos
  getSourceLine (If       pos _ _ _) = pos
  getSourceLine (UnaryOp pos _ _   ) = pos
  getSourceLine (Seq (x :| _)      ) = getSourceLine x
  getSourceLine (Lambda pos _ _    ) = pos
  getSourceLine (Let pos _ _ _     ) = pos

data BinaryOperator = Add | Subtract | Multiply | Divide | Mod
                    | Equal | Inequal
                    | LessThan | GreaterThan
                    | LessThanEqual | GreaterThanEqual
    deriving (Eq, Show)

data UnaryOperator = Positive | Negative
    deriving (Eq, Show)
