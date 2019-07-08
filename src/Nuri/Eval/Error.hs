module Nuri.Eval.Error where

import           Data.Text

import           Text.Megaparsec.Pos

data Error = UnboundSymbol SourcePos Text
           | BoundSymbol SourcePos Text
           | OperateTypeError SourcePos Text Text
           | NotCallable SourcePos Text
           | NotInFunction SourcePos
  deriving (Eq, Show)

