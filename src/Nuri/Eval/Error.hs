module Nuri.Eval.Error where

import           Data.Text

import           Text.Megaparsec.Pos

data Error = UnboundSymbol SourcePos Text
           | OperateTypeError SourcePos Text Text
  deriving (Eq, Show)

