module Nuri.Expr where


import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.ASTNode
import           Nuri.Literal

data Expr = -- 리터럴 표현식 : 코드 위치, 리터럴 값
              Lit SourcePos Literal
            -- 변수 읽기 표현식 : 코드 위치, 변수명 
            | Var SourcePos Text
            -- 함수 호출 표현식 : 코드 위치, 함수 이름, 함수 인자 리스트
            | FuncCall SourcePos Text [Expr]
            -- 이항 연산 표현식 : 코드 위치, 이항 연산자, 피연산자(좌), 피연산자(우)
            | BinaryOp SourcePos BinaryOperator Expr Expr
            -- 단항 연산 표현식 : 코드 위치, 단항 연산자, 피연산자
            | UnaryOp SourcePos UnaryOperator Expr
          deriving (Show)

instance Eq Expr where
  Lit _ v1         == Lit _ v2         = v1 == v2
  Var _ v1         == Var _ v2         = v1 == v2
  FuncCall _ f1 a1 == FuncCall _ f2 a2 = (f1 == f2) && (a1 == a2)
  BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 =
    (op1 == op2) && (l1 == l2) && (r1 == r2)
  UnaryOp _ op1 v1 == UnaryOp _ op2 v2 = (op1 == op2) && (v1 == v2)
  _                == _                = False

instance ASTNode Expr where
  srcPos (Lit pos _         ) = pos
  srcPos (Var pos _         ) = pos
  srcPos (FuncCall pos _ _  ) = pos
  srcPos (BinaryOp pos _ _ _) = pos
  srcPos (UnaryOp pos _ _   ) = pos

data BinaryOperator = Add | Subtract | Multiply | Divide | Mod
                    | Equal | Inequal
                    | LessThan | GreaterThan
                    | LessThanEqual | GreaterThanEqual
    deriving (Eq, Show)

data UnaryOperator = Positive | Negative
    deriving (Eq, Show)
