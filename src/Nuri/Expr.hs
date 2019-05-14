module Nuri.Expr where

import Text.Megaparsec.Pos

data Literal = LitInteger Integer
             | LitDouble Double
    deriving(Eq)

instance Show Literal where
    show (LitInteger v) = "(LitInteger " ++ show v ++ ")"
    show (LitDouble v) = "(LitDouble " ++ show v ++ ")"
