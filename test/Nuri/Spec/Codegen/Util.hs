module Nuri.Spec.Codegen.Util where

import           Test.Hspec

import           Control.Monad.RWS
import qualified Data.Set.Ordered              as S

import           Haneul.Builder
import           Haneul.Instruction


shouldBuild :: Builder () -> (BuilderInternal, [Instruction]) -> Expectation
shouldBuild actual expected = do
  let (result, internal, insts) =
        runRWS (runExceptT actual) () (BuilderInternal S.empty [])
  case result of
    Left err ->
      expectationFailure
        $  "expected bytecodes, but caught an error: "
        ++ show err
    Right () -> (internal, snd <$> insts) `shouldBe` expected
