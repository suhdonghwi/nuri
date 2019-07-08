module Nuri.ASTNode where

import Text.Megaparsec.Pos

class ASTNode a where
  srcPos :: a -> SourcePos
