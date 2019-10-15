module Haneul.Constant where

import           Control.Lens                             ( makeLenses )
import           Control.Lens.TH                          ( )
import           Data.Set.Ordered                         ( OSet )

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Haneul.Instruction

data Constant = ConstInteger Integer
              | ConstReal Double
              | ConstChar Char
              | ConstBool Bool
              | ConstFunc FuncObject
  deriving (Eq, Show, Ord)

data FuncObject = FuncObject { _arity :: Integer,
                               _insts :: [(SourcePos, Instruction)],
                               _funcConstTable :: OSet Constant,
                               _funcVarNames :: OSet Text
                             }
  deriving (Eq, Show, Ord)

$(makeLenses ''FuncObject)

