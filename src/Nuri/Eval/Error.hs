module Nuri.Eval.Error where

import           Data.Text

import           Text.Megaparsec.Pos

data Error = UnboundSymbol SourcePos Text
  deriving (Eq, Show)

