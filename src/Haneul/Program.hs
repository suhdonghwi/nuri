module Haneul.Program where

import           Control.Lens                             ( makeLenses
                                                          , view
                                                          )
import           Control.Monad.RWS                        ( execRWS )

import           Haneul.Builder
import           Haneul.Instruction

data Program = Program { _programConstTable :: ConstTable, _programCode :: Code }
  deriving (Eq, Show)

toProgram :: Builder () -> Program
toProgram result =
  let (internal, code) = execRWS result 0 defaultInternal
  in  Program (view internalConstTable internal) (clearMarks internal code)

$(makeLenses ''Program)
