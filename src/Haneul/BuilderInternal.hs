module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Constant
import           Haneul.ConstTable

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable, _internalVarNames :: OSet (String, Int), _internalOffset :: Int32, _internalMarks :: [Int32] }
  deriving (Show)

instance Eq BuilderInternal where
  BuilderInternal t1 v1 _ m1 == BuilderInternal t2 v2 _ m2 =
    (t1 == t2) && (v1 == v2) && (m1 == m2)

$(makeLenses ''BuilderInternal)

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal (S.singleton ConstNone) S.empty 0 []
