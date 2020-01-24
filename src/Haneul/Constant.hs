module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Instruction                       ( Code )

data Constant = ConstNone
              | ConstInteger Int64
              | ConstReal Double
              | ConstString String
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)

type ConstTable = OSet Constant

data FuncObject = FuncObject { _funcArity :: Word16,
                               _funcBody :: Code,
                               _funcConstTable :: ConstTable
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

