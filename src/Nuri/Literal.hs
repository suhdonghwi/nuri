module Nuri.Literal where


data Literal = LitInteger Integer
             | LitReal Double
             | LitChar Char
             | LitBool Bool
    deriving(Eq, Show)
