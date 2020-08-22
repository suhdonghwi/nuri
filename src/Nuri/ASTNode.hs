module Nuri.ASTNode where

import Text.Megaparsec.Pos (SourcePos)

class ASTNode a where
  getSourcePos :: a -> SourcePos
