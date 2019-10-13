module Haneul.Constant where

data Constant = ConstInteger Integer
              | ConstReal Double
              | ConstChar Char
              | ConstBool Bool
  deriving (Eq, Show, Ord)
