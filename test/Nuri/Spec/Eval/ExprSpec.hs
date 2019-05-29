module Nuri.Spec.Eval.ExprSpec where

import           Test.Hspec

import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Data.Map.Strict

import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Expr
import           Nuri.Eval.Val

import           Nuri.Spec.Util
import           Nuri.Spec.Parse.Util

testEvalWith :: Expr -> SymbolTable -> Either Error (Val, SymbolTable)
testEvalWith expr table = runExcept (runStateT (evalExpr expr) table)

testEval :: Expr -> Either Error (Val, SymbolTable)
testEval expr = testEvalWith expr empty

sampleTable :: SymbolTable
sampleTable = fromList [("나이", IntegerVal 17)]

spec :: Spec
spec = do
  describe "리터럴 평가" $ do
    describe "정수 리터럴 평가" $ do
      it "LitInteger 10을 IntegerVal 10으로 평가" $ do
        testEval (litInteger 10) `shouldBe` Right (IntegerVal 10, empty)

  describe "변수 식별자 평가" $ do
    it "심볼 테이블에서 식별자 값 가져오기" $ do
      testEvalWith (var "나이") sampleTable
        `shouldBe` Right (IntegerVal 17, sampleTable)
    it "없는 심볼에 대해서 UnboundSymbol 에러" $ do
      testEvalWith (var "사과") sampleTable
        `shouldBe` Left (UnboundSymbol initPos "사과")
