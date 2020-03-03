module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Nuri.Spec.Util

import           Haneul.Builder
import           Haneul.BuilderInternal
import           Haneul.Instruction
import           Haneul.Program
import           Haneul.Constant

shouldBuild :: Builder a -> (ConstTable, [Instruction]) -> Expectation
shouldBuild actual expected =
  let Program { _programConstTable = constTable, _programCode = insts } =
          toProgram defaultInternal actual
  in  (constTable, snd <$> insts) `shouldBe` expected

defaultI :: BuilderInternal
defaultI = defaultInternal

loadGlobal :: Word32 -> Instruction
loadGlobal = LoadGlobal . (+ genericLength defaultGlobalNames)

storeGlobal :: Word32 -> Instruction
storeGlobal = StoreGlobal . (+ genericLength defaultGlobalNames)

ann :: [Instruction] -> [Ann Instruction]
ann = fmap (initPos, )
