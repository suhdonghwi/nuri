module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Control.Monad.RWS
import qualified Data.Set.Ordered              as S

import           Nuri.Literal
import           Haneul.Builder
import           Haneul.Instruction


shouldBuild :: Builder () -> (S.OSet Literal, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let result = execRWS actual () S.empty
  result `shouldBe` expected
