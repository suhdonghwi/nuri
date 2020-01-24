module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Nuri.Spec.Util

import           Haneul.Builder
import           Haneul.BuilderInternal
import           Haneul.Instruction
import           Haneul.Program
import           Haneul.Constant

shouldBuild :: Builder () -> (ConstTable, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let Program { _programConstTable = constTable, _programCode = insts } =
        toProgram actual
  (constTable, snd <$> insts) `shouldBe` expected

defaultI :: BuilderInternal
defaultI = defaultInternal

ann :: [Instruction] -> [Ann Instruction]
ann = fmap (initPos, )
