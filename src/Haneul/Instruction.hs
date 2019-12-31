module Haneul.Instruction where

data Instruction = Push Int | Pop
                 | Store Int | Load Int | LoadBuiltin Int
                 | Label Int
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | Inequal | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual
                 | Negate
                 deriving (Eq, Show, Ord)
