module Haneul.Instruction where

data Instruction = Push Int
                 | Add
                 | Subtract
                 | Multiply
                 | Divide
                 deriving (Eq, Show)
