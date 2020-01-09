module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Instruction                       ( Code )

data Constant = ConstInteger Integer
              | ConstReal Double
              | ConstString String
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)


data FuncObject = FuncObject { _funcArity :: Word16,
                               _funcBody :: Code,
                               _funcConstTable :: OSet Constant,
                               _funcVarNames :: OSet String
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

