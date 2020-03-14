module Haneul.BuilderInternal where

import           Control.Lens                             ( makeLenses )

import qualified Data.Set.Ordered              as S

import           Haneul.Constant

data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable,
                                         _internalDepth :: Word8,
                                         _internalLocalVars :: [(Word8, Text)],
                                         _internalMaxLocalCount :: Word32,
                                         _internalGlobalVarNames :: S.OSet Text,
                                         _internalFreeVars :: S.OSet (Word8, Word8),
                                         _internalOffset :: Word32,
                                         _internalMarks :: [Word32]
                                       }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

defaultGlobalNames :: [Text]
defaultGlobalNames = ["보여주다"]

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal
  { _internalConstTable     = S.empty
  , _internalDepth          = 0
  , _internalLocalVars      = []
  , _internalMaxLocalCount  = 0
  , _internalGlobalVarNames = S.fromList defaultGlobalNames
  , _internalFreeVars       = S.empty
  , _internalOffset         = 0
  , _internalMarks          = []
  }
