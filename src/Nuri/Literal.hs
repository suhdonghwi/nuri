module Nuri.Literal where

data Literal
  = LitNone
  | LitInteger Int64
  | LitReal Double
  | LitChar Char
  | LitBool Bool
  deriving (Eq, Show, Ord)
