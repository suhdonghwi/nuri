module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Nuri.Spec.Util

import           Control.Monad.RWS

import           Haneul.Builder
import           Haneul.Instruction


shouldBuild :: Builder () -> (BuilderInternal, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let (internal, insts) = execRWS actual 0 defaultInternal
  (internal, snd <$> insts) `shouldBe` expected

defaultI :: BuilderInternal
defaultI = defaultInternal

ann :: [Instruction] -> [AnnInstruction]
ann = fmap (initPos, )
