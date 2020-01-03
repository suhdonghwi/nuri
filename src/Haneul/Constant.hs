module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Data.Set.Ordered                         ( OSet )
import           Text.Megaparsec.Pos                      ( Pos )

import           Haneul.Instruction                       ( Instruction )

data Constant = ConstInteger Integer
              | ConstReal Double
              | ConstChar Char
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)


data FuncObject = FuncObject { _arity :: Integer,
                               _insts :: [(Pos, Instruction)],
                               _funcConstTable :: OSet Constant,
                               _funcVarNames :: OSet Text
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

