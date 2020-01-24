module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Constant

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable, _internalVarNames :: OSet (String, Int), _internalOffset :: Int32, _internalMarks :: [Int32] }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal (S.singleton ConstNone) S.empty 0 []
