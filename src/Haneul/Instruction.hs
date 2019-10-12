module Haneul.Instruction where

data Instruction = Push Int | Pop
                 | Add | Subtract | Multiply | Divide | Mod
                 | Negate
                 deriving (Eq, Show)
