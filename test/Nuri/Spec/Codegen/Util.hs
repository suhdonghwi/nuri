module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Control.Monad.RWS

import           Haneul.Builder
import           Haneul.Instruction


shouldBuild :: Builder () -> (BuilderInternal, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let (internal, insts) = execRWS actual "(test)" defaultInternal
  (internal, snd <$> insts) `shouldBe` expected

defaultI :: BuilderInternal
defaultI = defaultInternal
