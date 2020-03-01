module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S

import           Haneul.Constant

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable,
                                         _internalVarNames :: S.OSet String,
                                         _internalGlobalVarNames :: S.OSet String,
                                         _internalFreeVars :: S.OSet (Word8, Word8),
                                         _internalOffset :: Word32,
                                         _internalMarks :: [Word32]
                                       }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultGlobalNames :: [String]
defaultGlobalNames = ["보여주다"]

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal
  { _internalConstTable     = S.empty
  , _internalVarNames       = S.empty
  , _internalGlobalVarNames = S.fromList defaultGlobalNames
  , _internalFreeVars       = S.empty
  , _internalOffset         = 0
  , _internalMarks          = []
  }
