module Haneul.Instruction where

data Instruction = Push Int
                 | Add
                 | Subtract
                 | Multiply
                 | Divide
                 | Mod
                 deriving (Eq, Show)
