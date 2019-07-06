module Nuri.Expr where

import           Data.Text

import           Text.Megaparsec.Pos

import           Nuri.ASTNode

data Literal = LitInteger Integer
    deriving(Eq)

instance Show Literal where
    show (LitInteger v) = "(LitInteger " ++ show v ++ ")"

data Expr = Lit SourcePos Literal
          | Var SourcePos Text
          | App SourcePos Expr [Expr]
          | BinaryOp SourcePos Op Expr Expr
          | UnaryOp SourcePos Op Expr
    deriving(Show)

instance Eq Expr where
    Lit _ v1    == Lit _ v2    = v1 == v2
    Var _ v1    == Var _ v2    = v1 == v2
    App _ f1 a1 == App _ f2 a2 = (f1 == f2) && (a1 == a2)
    BinaryOp _ op1 l1 r1 == BinaryOp _ op2 l2 r2 =
        (op1 == op2) && (l1 == l2) && (r1 == r2)
    UnaryOp _ op1 v1 == UnaryOp _ op2 v2 = (op1 == op2) && (v1 == v2)
    _                == _                = False

instance ASTNode Expr where
    srcPos (Lit pos _         ) = pos
    srcPos (Var pos _         ) = pos
    srcPos (App pos _ _       ) = pos
    srcPos (BinaryOp pos _ _ _) = pos
    srcPos (UnaryOp pos _ _   ) = pos

data Op = Plus | Minus | Asterisk | Slash
    deriving(Eq, Show)
