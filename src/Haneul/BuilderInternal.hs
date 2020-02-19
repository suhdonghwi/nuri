module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S

import           Haneul.Constant

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable,
                                         _internalVarNames :: S.OSet String,
                                         _internalOffset :: Word32,
                                         _internalMarks :: [Word32]
                                       }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal (S.empty) (S.empty) 0 []
