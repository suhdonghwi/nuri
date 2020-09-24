module Nuri.Expr where

import Nuri.ASTNode (ASTNode (..))
import Nuri.Literal (Literal)
import Text.Megaparsec.Pos (SourcePos)

data FuncKind = Normal | Verb | Adjective
  deriving (Eq, Show)

data FuncVariation = Synonym Text | Antonym Text
  deriving (Eq, Show)

data DeclType
  = FuncDecl [(Text, Text)] (Maybe Expr)
  | VerbDecl [(Text, Text)] (Maybe Expr)
  | AdjectiveDecl [(Text, Text)] [FuncVariation] (Maybe Expr)
  | ConstDecl Expr
  | StructDecl [Text]
  deriving (Eq, Show)

data Decl = Decl SourcePos Text DeclType
  deriving (Show)

instance Eq Decl where
  Decl _ n1 t1 == Decl _ n2 t2 = (n1 == n2) && (t1 == t2)

instance ASTNode Decl where
  getSourcePos (Decl pos _ _) = pos

data Expr -- 리터럴 표현식 : 코드 위치, 리터럴 값
  = Lit SourcePos Literal
  | -- 변수 읽기 표현식 : 코드 위치, 변수명
    Var SourcePos Text
  | -- 함수 호출 표현식 : 코드 위치, 함수식, 함수 인자 리스트
    FuncCall SourcePos Expr [(Expr, Text)]
  | -- 조건 분기 표현식 : 코드 위치, 조건식, then문, else문
    If SourcePos Expr Expr Expr
  | -- 이항 연산 표현식 : 코드 위치, 이항 연산자, 피연산자(좌), 피연산자(우)
    BinaryOp SourcePos BinaryOperator Expr Expr
  | -- 단항 연산 표현식 : 코드 위치, 단항 연산자, 피연산자
    UnaryOp SourcePos UnaryOperator Expr
  | -- 표현식 시퀀스 : 표현식 목록
    Seq (NonEmpty (Either Decl Expr))
  | -- 람다 표현식 : 코드 위치, 함수 이름, 인수 목록, 함수 본문
    Lambda SourcePos Text [(Text, Text)] Expr
  | -- 구조체 표현식 : 코드 위치, 구조체 이름, 필드 & 값 쌍
    Struct SourcePos Text [(Text, Expr)]
  deriving (Show)

instance Eq Expr where
  Lit _ v1 == Lit _ v2 = v1 == v2
  Var _ v1 == Var _ v2 = v1 == v2
  FuncCall _ f1 a1 == FuncCall _ f2 a2 = (f1 == f2) && (a1 == a2)
  If _ c1 t1 e1 == If _ c2 t2 e2 = (c1 == c2) && (t1 == t2) && (e1 == e2)
  BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 =
    (op1 == op2) && (l1 == l2) && (r1 == r2)
  UnaryOp _ op1 v1 == UnaryOp _ op2 v2 = (op1 == op2) && (v1 == v2)
  Seq e1 == Seq e2 = e1 == e2
  Lambda _ n1 a1 b1 == Lambda _ n2 a2 b2 = (n1 == n2) && (a1 == a2) && (b1 == b2)
  Struct _ a1 b1 == Struct _ a2 b2 = (a1 == a2) && (b1 == b2)
  _ == _ = False

instance ASTNode Expr where
  getSourcePos (Lit pos _) = pos
  getSourcePos (Var pos _) = pos
  getSourcePos (FuncCall pos _ _) = pos
  getSourcePos (BinaryOp pos _ _ _) = pos
  getSourcePos (If pos _ _ _) = pos
  getSourcePos (UnaryOp pos _ _) = pos
  getSourcePos (Seq (x :| _)) = case x of
    Left (Decl pos _ _) -> pos
    Right expr -> getSourcePos expr
  getSourcePos (Lambda pos _ _ _) = pos
  getSourcePos (Struct pos _ _) = pos

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Equal
  | Inequal
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | LogicAnd
  | LogicOr
  deriving (Eq, Show)

data UnaryOperator = Positive | Negative | LogicNot
  deriving (Eq, Show)
