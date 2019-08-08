module Nuri.Eval.ValType where

data ValType = IntegerType | RealType | CharType | BoolType | FuncType | UndefinedType
  deriving (Eq, Show)
