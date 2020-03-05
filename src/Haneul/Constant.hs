module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Instruction                       ( Code )

data Constant = ConstNone
              | ConstInteger Int64
              | ConstReal Double
              | ConstChar Char
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)

type ConstTable = OSet Constant

data FuncObject = FuncObject { _funcJosa :: [Text],
                               _funcMaxStackSize :: Word64,
                               _funcMaxLocalCount :: Word32,
                               _funcConstTable :: ConstTable,
                               _funcBody :: Code
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

