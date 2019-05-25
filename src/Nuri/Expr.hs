module Nuri.Expr where

import           Data.Text

import           Text.Megaparsec.Pos

data Literal = LitInteger Integer
             | LitDouble Double
    deriving(Eq)

instance Show Literal where
    show (LitInteger v) = "(LitInteger " ++ show v ++ ")"
    show (LitDouble v) = "(LitDouble " ++ show v ++ ")"

data Expr = Lit SourcePos Literal
          | Var SourcePos Text
          | BinaryOp SourcePos Op Expr Expr
          | UnaryOp SourcePos Op Expr
    deriving(Show)

instance Eq Expr where
    Lit _ v1 == Lit _ v2 = v1 == v2
    Var _ v1 == Var _ v2 = v1 == v2
    BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 = (op1 == op2) && (l1 == l2) && (r1 == r2)
    UnaryOp _ op1 v1 == UnaryOp _ op2 v2 = (op1 == op2) && (v1 == v2)
    _ == _ = False

data Op = Plus | Minus | Asterisk | Slash
    deriving(Eq, Show)

srcPos :: Expr -> SourcePos
srcPos (Lit pos _) = pos
srcPos (Var pos _) = pos
