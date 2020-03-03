module Haneul.Program where

import           Control.Lens                             ( makeLenses
                                                          , view
                                                          )
import           Control.Monad.RWS                        ( execRWS )

import           Haneul.Builder
import           Haneul.BuilderInternal
import           Haneul.Instruction
import           Haneul.Constant

data Program = Program { _programGlobalVarNames :: [String],
                         _programStackSize :: Word64,
                         _programConstTable :: ConstTable,
                         _programCode :: Code
                       }
  deriving (Eq, Show)

toProgram :: BuilderInternal -> Builder () -> Program
toProgram internal result =
  let (internal', code') = execRWS result [] internal
      code               = clearMarks internal' code'
  in  Program
        { _programGlobalVarNames = toList
                                     $ view internalGlobalVarNames internal'
        , _programStackSize      = estimateStackSize code
        , _programConstTable     = view internalConstTable internal'
        , _programCode           = code
        }

$(makeLenses ''Program)
