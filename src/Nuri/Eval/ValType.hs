module Nuri.Eval.ValType where

data ValType = IntegerType | RealType | BoolType | FuncType | UndefinedType
  deriving (Eq, Show)
