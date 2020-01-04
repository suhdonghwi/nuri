module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Instruction                       ( AnnInstruction )

data Constant = ConstInteger Integer
              | ConstReal Double
              | ConstChar Char
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)


data FuncObject = FuncObject { _arity :: Word16,
                               _insts :: [AnnInstruction],
                               _funcConstTable :: OSet Constant,
                               _funcVarNames :: OSet Text
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

