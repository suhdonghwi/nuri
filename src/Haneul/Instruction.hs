module Haneul.Instruction where

data Instruction = Push Int | Pop
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Negate
                 deriving (Eq, Show)
