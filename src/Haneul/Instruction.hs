module Haneul.Instruction where

data Instruction = Push Int | Pop
                 | Store Int | Load Int
                 | Label Int | Call Int
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | Inequal | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual
                 | Negate
                 deriving (Eq, Show, Ord)
