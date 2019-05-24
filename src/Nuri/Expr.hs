module Nuri.Expr where

import           Text.Megaparsec.Pos

data Literal = LitInteger Integer
             | LitDouble Double
    deriving(Eq)

instance Show Literal where
    show (LitInteger v) = "(LitInteger " ++ show v ++ ")"
    show (LitDouble v) = "(LitDouble " ++ show v ++ ")"

data Expr = Lit SourcePos Literal
          | Var SourcePos String
          | BinaryOp SourcePos Op Expr Expr
    deriving(Show)

instance Eq Expr where
    Lit _ v1 == Lit _ v2 = v1 == v2
    Var _ v1 == Var _ v2 = v1 == v2
    BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 = (op1 == op2) && (l1 == l2) && (r1 == r2)

data Op = Add | Subtract | Multiply | Divide
    deriving(Eq, Show)

srcPos :: Expr -> SourcePos
srcPos (Lit pos _) = pos
srcPos (Var pos _) = pos
