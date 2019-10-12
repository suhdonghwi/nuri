module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Control.Monad.RWS
import qualified Data.Set.Ordered              as S

import           Haneul.Builder
import           Haneul.Instruction


shouldBuild :: Builder () -> (BuilderInternal, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let result = execRWS actual () (BuilderInternal S.empty [])
  (second (fmap snd) result) `shouldBe` expected
