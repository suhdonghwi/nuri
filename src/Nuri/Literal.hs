module Nuri.Literal where


data Literal = LitNone
             | LitInteger Integer
             | LitReal Double
             | LitString String
             | LitBool Bool
    deriving(Eq, Show, Ord)
