module Nuri.ASTNode where

import           Text.Megaparsec.Pos                      ( Pos )

class ASTNode a where
  getSourceLine :: a -> Pos

