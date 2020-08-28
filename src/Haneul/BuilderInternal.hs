module Haneul.BuilderInternal where

import Control.Lens (makeLenses)
import qualified Data.Set.Ordered as S (OSet, empty)
import Haneul.Constant (ConstTable)
import Nuri.Expr (DeclKind (VerbDecl))
import Text.Megaparsec.Pos (Pos, mkPos)

data BuilderInternal = BuilderInternal
  { _internalConstTable :: ConstTable,
    _internalDepth :: Word8,
    _internalLocalVars :: S.OSet (Word8, Text),
    _internalMaxLocalCount :: Word32,
    _internalGlobalVarNames :: S.OSet Text,
    _internalFreeVars :: S.OSet (Word8, Word8),
    _internalOffset :: Word32,
    _internalMarks :: [Word32],
    _internalLastLine :: Pos,
    _internalLastFilePath :: String
  }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultDecls :: [(DeclKind, Text)]
defaultDecls = [(VerbDecl, "보여주다")]

defaultInternal :: BuilderInternal
defaultInternal =
  BuilderInternal
    { _internalConstTable = S.empty,
      _internalDepth = 0,
      _internalLocalVars = S.empty,
      _internalMaxLocalCount = 0,
      _internalGlobalVarNames = S.empty,
      _internalFreeVars = S.empty,
      _internalOffset = 0,
      _internalMarks = [],
      _internalLastLine = mkPos 1,
      _internalLastFilePath = ""
    }
