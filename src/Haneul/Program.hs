module Haneul.Program where

import           Control.Lens                             ( makeLenses
                                                          , view
                                                          )
import           Control.Monad.RWS                        ( runRWS )

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

toProgram :: BuilderInternal -> Builder Word64 -> Program
toProgram internal result =
  let (stackSize, internal', code) = runRWS result [] internal
  in  Program
        { _programGlobalVarNames = toList
                                     $ view internalGlobalVarNames internal'
        , _programStackSize      = stackSize
        , _programConstTable     = view internalConstTable internal'
        , _programCode           = clearMarks internal' code
        }

$(makeLenses ''Program)
