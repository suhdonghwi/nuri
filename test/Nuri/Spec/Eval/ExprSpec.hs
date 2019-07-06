module Nuri.Spec.Eval.ExprSpec where

import           Test.Hspec

import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Data.Map.Strict

import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Expr
import           Nuri.Eval.Val

import           Nuri.Spec.Parse.Util
import           Nuri.Spec.Eval.Util

testEvalWith :: Expr -> SymbolTable -> Either Error (Val, SymbolTable)
testEvalWith expr table = runExcept (runStateT (evalExpr expr) table)

testEval :: Expr -> Either Error (Val, SymbolTable)
testEval expr = testEvalWith expr empty

sampleTable :: SymbolTable
sampleTable = fromList [("나이", IntegerVal 17), ("십", FuncVal sampleFunc)]

sampleFunc :: [Val] -> Eval Val
sampleFunc _ = return (IntegerVal 10)

spec :: Spec
spec = do
  describe "리터럴 평가" $ do
    describe "정수 리터럴 평가" $ do
      it "LitInteger 10을 IntegerVal 10으로 평가" $ do
        testEval (litInteger 10) `shouldEval` (IntegerVal 10, empty)

  describe "변수 식별자 평가" $ do
    it "심볼 테이블에서 식별자 값 가져오기" $ do
      testEvalWith (var "나이") sampleTable
        `shouldEval` (IntegerVal 17, sampleTable)
    it "없는 심볼에 대해서 UnboundSymbol 에러" $ do
      testEvalWith (var "사과") sampleTable `shouldEvalError` unboundSymbol "사과"

  describe "이항 연산자 평가" $ do
    it "정수 두 개 더하기" $ do
      testEval (binaryOp Plus (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 15, empty)
    it "정수 두 개 곱하기" $ do
      testEval (binaryOp Asterisk (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 50, empty)
    it "정수 두 개 나누기" $ do
      testEval (binaryOp Slash (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 2, empty)
    it "정수형 변수와 정수 리터럴 더하기" $ do
      testEvalWith (binaryOp Plus (var "나이") (litInteger 5)) sampleTable
        `shouldEval` (IntegerVal 22, sampleTable)
    it "정수와 함수 더했을 시 타입 에러" $ do
      testEvalWith (binaryOp Plus (litInteger 10) (var "십")) sampleTable
        `shouldEvalError` operateTypeError "정수" "함수"

