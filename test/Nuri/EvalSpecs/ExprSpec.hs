module Nuri.EvalSpecs.ExprSpec where

import           Test.Hspec

import           Control.Monad.State.Lazy
import           Data.Map.Strict

import           Nuri.Eval.Expr
import           Nuri.Expr
import           Nuri.Eval.Val
import           Nuri.Util

testEval :: Expr -> (Val, SymbolTable)
testEval expr = runState (evalExpr expr) empty

testEvalWith :: Expr -> SymbolTable -> (Val, SymbolTable)
testEvalWith expr = runState (evalExpr expr)

sampleTable :: SymbolTable
sampleTable = fromList [("나이", IntegerVal 17)]

spec :: Spec
spec = do
  describe "리터럴 평가" $ do
    describe "정수 리터럴 평가" $ do
      it "LitInteger 10을 IntegerVal 10으로 평가" $ do
        testEval (litInteger 10) `shouldBe` (IntegerVal 10, empty)

  describe "변수 식별자 평가" $ do
    it "심볼 테이블에서 식별자 값 가져오기" $ do
      testEvalWith (var "나이") sampleTable
        `shouldBe` (IntegerVal 17, sampleTable)
