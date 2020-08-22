module Nuri.Spec.Codegen.Util where

import qualified Data.Set.Ordered as S
import Haneul.Builder
import Haneul.BuilderInternal
import Haneul.Constant
import Haneul.Instruction
import Test.Hspec
import Text.Megaparsec.Pos (mkPos)

shouldBuild :: Builder () -> (ConstTable, [Instruction]) -> Expectation
shouldBuild actual expected =
  let FuncObject {_funcConstTable = constTable, _funcCode = insts} =
        internalToFuncObject (runBuilder defaultInternal actual)
   in (constTable, insts) `shouldBe` expected

defaultI :: BuilderInternal
defaultI = defaultInternal

funcObject :: FuncObject
funcObject =
  FuncObject
    { _funcCode = [],
      _funcConstTable = S.empty,
      _funcGlobalVarNames = S.empty,
      _funcJosa = [],
      _funcMaxLocalCount = 0,
      _funcLineNo = mkPos 1,
      _funcLineNoTable = [],
      _funcName = "",
      _funcStackSize = 0
    }
