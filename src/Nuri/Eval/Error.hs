module Nuri.Eval.Error where

import           Data.Text

import           Text.Megaparsec.Pos

data Error = UnboundSymbol SourcePos Text
           | BoundSymbol SourcePos Text
           | OperateTypeError SourcePos Text Text
           | NotCallable SourcePos Text
           | NotInFunction SourcePos
           | IncorrectArgsNum SourcePos Int Int -- 실제 arity, 주어진 arity 순
  deriving (Eq, Show)
