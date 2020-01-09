module Nuri.Literal where


data Literal = LitInteger Integer
             | LitReal Double
             | LitString String
             | LitBool Bool
    deriving(Eq, Show, Ord)
