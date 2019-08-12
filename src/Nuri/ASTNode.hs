module Nuri.ASTNode where

import           Text.Megaparsec.Pos                      ( SourcePos )

class ASTNode a where
  srcPos :: a -> SourcePos
