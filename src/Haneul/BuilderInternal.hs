module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S

import           Haneul.Constant

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable, _internalOffset :: Int32, _internalMarks :: [Int32] }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal (S.empty) 0 []
