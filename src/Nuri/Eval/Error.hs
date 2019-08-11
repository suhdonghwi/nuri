module Nuri.Eval.Error where

import           Text.Megaparsec.Pos

import           Nuri.Eval.ValType

data Error = UnboundSymbol SourcePos Text
           | BoundSymbol SourcePos Text
           | OperateTypeError SourcePos [ValType]
           | NotConditionType SourcePos ValType
           | DivideByZero SourcePos
           | NotCallable SourcePos ValType
           | NotInFunction SourcePos
           | IncorrectArgsNum SourcePos Int Int -- 실제 arity, 주어진 arity 순
  deriving (Eq, Show)
