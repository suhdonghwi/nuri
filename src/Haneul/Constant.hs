module Haneul.Constant where

import Control.Lens (makeLenses)
import Data.Set.Ordered (OSet)
import Haneul.Instruction (Code)
import Text.Megaparsec.Pos (Pos)

data Constant
  = ConstNone
  | ConstInteger Int64
  | ConstReal Double
  | ConstChar Char
  | ConstBool Bool
  | ConstFunc FuncObject
  deriving (Eq, Show, Ord)

type ConstTable = OSet Constant

-- (n, m)은 n번째 줄 일때 줄 번호를 m만큼 증가하라는 의미
type LineNoTable = [(Word32, Word32)]

data FuncObject = FuncObject
  { _funcJosa :: [Text],
    _funcGlobalVarNames :: OSet Text,
    _funcStackSize :: Word64,
    _funcMaxLocalCount :: Word32,
    _funcConstTable :: ConstTable,
    _funcLineNo :: Pos,
    _funcLineNoTable :: LineNoTable,
    _funcCode :: Code
  }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)
