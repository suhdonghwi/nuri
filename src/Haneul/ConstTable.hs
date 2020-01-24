module Haneul.ConstTable where

import           Data.Set.Ordered                         ( OSet )

import           Haneul.Constant

type ConstTable = OSet Constant
