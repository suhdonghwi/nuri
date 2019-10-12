module Haneul.Instruction where

data Instruction = Push Int | Pop
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | Inequal | LessThan | GreaterThan | LessThanEqual | GreaterThanEqual
                 | Negate
                 deriving (Eq, Show)
