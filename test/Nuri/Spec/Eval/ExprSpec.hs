module Nuri.Spec.Eval.ExprSpec where

import           Test.Hspec

import           Control.Lens                      hiding ( assign )

import qualified Data.Map                      as M

import           Nuri.Expr
import           Nuri.Eval.Val
import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.ValType

import           Nuri.Spec.Util
import           Nuri.Spec.Eval.Util

testExprEvalWith
  :: Expr -> InterpreterState -> IO (Either Error (Val, InterpreterState))
testExprEvalWith = testEvalWith evalExpr

testExprEval :: Expr -> IO (Either Error (Val, InterpreterState))
testExprEval = testEval evalExpr

spec :: Spec
spec = do
  describe "리터럴 평가" $ do
    describe "정수 리터럴 평가" $ do
      it "LitInteger 10을 IntegerVal 10으로 평가" $ do
        testExprEval (litInteger 10) `shouldEval` (IntegerVal 10, initState)
    describe "실수 리터럴 평가" $ do
      it "LitReal 10을 RealVal 10으로 평가" $ do
        testExprEval (litReal 10) `shouldEval` (RealVal 10, initState)
    describe "부울 리터럴 평가" $ do
      it "LitBool True를 BoolVal True로 평가" $ do
        testExprEval (litBool True) `shouldEval` (BoolVal True, initState)

  describe "변수 식별자 평가" $ do
    it "심볼 테이블에서 식별자 값 가져오기" $ do
      testExprEvalWith (var "나이") sampleState
        `shouldEval` (IntegerVal 17, sampleState)
    it "없는 심볼에 대해서 UnboundSymbol 에러" $ do
      testExprEvalWith (var "사과") sampleState
        `shouldEvalError` unboundSymbol "사과"

  describe "이항 연산자 평가" $ do
    it "정수 두 개 더하기" $ do
      testExprEval (binaryOp Plus (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 15, initState)
    it "실수 두 개 더하기" $ do
      testExprEval (binaryOp Plus (litReal 10.5) (litReal 4.5))
        `shouldEval` (RealVal 15.0, initState)
    it "정수와 실수 더하기" $ do
      testExprEval (binaryOp Plus (litInteger 10) (litReal 4.5))
        `shouldEval` (RealVal 14.5, initState)
    it "정수와 함수 더했을 시 타입 에러" $ do
      testExprEvalWith (binaryOp Plus (litInteger 10) (var "십")) sampleState
        `shouldEvalError` operateTypeError [IntegerType, FuncType]
    it "정수 두 개 곱하기" $ do
      testExprEval (binaryOp Asterisk (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 50, initState)
    it "실수 두 개 곱하기" $ do
      testExprEval (binaryOp Asterisk (litReal 10.0) (litReal 4.0))
        `shouldEval` (RealVal 40.0, initState)
    it "정수 두 개 나누기" $ do
      testExprEval (binaryOp Slash (litInteger 10) (litInteger 5))
        `shouldEval` (IntegerVal 2, initState)
    it "실수 두 개 나누기" $ do
      testExprEval (binaryOp Slash (litReal 5.0) (litReal 2.0))
        `shouldEval` (RealVal 2.5, initState)
    it "정수와 실수 나누기" $ do
      testExprEval (binaryOp Slash (litInteger 10) (litReal 2.0))
        `shouldEval` (RealVal 5.0, initState)
    it "0으로 나눌 시 DivideByZero 에러" $ do
      testExprEval (binaryOp Slash (litInteger 10) (litInteger 0))
        `shouldEvalError` divideByZero
    it "0으로 나눌 시 DivideByZero 에러 (실수)" $ do
      testExprEval (binaryOp Slash (litReal 10.0) (litReal 0.0))
        `shouldEvalError` divideByZero
    it "정수 두 개 나머지" $ do
      testExprEval (binaryOp Percent (litInteger 10) (litInteger 4))
        `shouldEval` (IntegerVal 2, initState)
    it "실수 두 개 나머지 구할 시 타입 에러" $ do
      testExprEval (binaryOp Percent (litReal 5.0) (litReal 2.0))
        `shouldEvalError` operateTypeError [RealType, RealType]
    it "정수와 실수 나머지 구할 시 타입 에러" $ do
      testExprEval (binaryOp Percent (litInteger 5) (litReal 2.0))
        `shouldEvalError` operateTypeError [IntegerType, RealType]
    it "0으로 나머지 구할 시 DivideByZero 에러" $ do
      testExprEval (binaryOp Percent (litInteger 10) (litInteger 0))
        `shouldEvalError` divideByZero
    it "정수 두 개 동등 비교" $ do
      testExprEval (binaryOp Equal (litInteger 10) (litInteger 10))
        `shouldEval` (BoolVal True, initState)
    it "정수 두 개 동등 비교 (같지 않음)" $ do
      testExprEval (binaryOp Equal (litInteger 10) (litInteger 4))
        `shouldEval` (BoolVal False, initState)
    it "정수 두 개 부등 비교" $ do
      testExprEval (binaryOp Inequal (litInteger 10) (litInteger 4))
        `shouldEval` (BoolVal True, initState)
    it "정수 두 개 부등 비교 (같음)" $ do
      testExprEval (binaryOp Inequal (litInteger 10) (litInteger 10))
        `shouldEval` (BoolVal False, initState)
    it "정수 두 개 대소 비교 (LT)" $ do
      testExprEval (binaryOp LessThan (litInteger 10) (litInteger 4))
        `shouldEval` (BoolVal False, initState)
    it "정수 두 개 대소 비교 (GT)" $ do
      testExprEval (binaryOp GreaterThan (litInteger 10) (litInteger 4))
        `shouldEval` (BoolVal True, initState)
    it "정수 두 개 대소 비교 (LTE)" $ do
      testExprEval (binaryOp LessThanEqual (litInteger 10) (litInteger 10))
        `shouldEval` (BoolVal True, initState)
    it "정수 두 개 대소 비교 (GTE)" $ do
      testExprEval (binaryOp GreaterThanEqual (litInteger 10) (litInteger 4))
        `shouldEval` (BoolVal True, initState)
    it "정수와 실수 대소 비교 (LT)" $ do
      testExprEval (binaryOp LessThan (litInteger 10) (litReal 4.5))
        `shouldEval` (BoolVal False, initState)
    it "정수와 실수 대소 비교 (GT)" $ do
      testExprEval (binaryOp GreaterThan (litInteger 10) (litReal 4.5))
        `shouldEval` (BoolVal True, initState)
    it "정수와 실수 대소 비교 (LTE)" $ do
      testExprEval (binaryOp LessThanEqual (litInteger 10) (litReal 9.5))
        `shouldEval` (BoolVal False, initState)
    it "정수와 실수 대소 비교 (GTE)" $ do
      testExprEval (binaryOp GreaterThanEqual (litInteger 10) (litReal 4.5))
        `shouldEval` (BoolVal True, initState)

  describe "단항 연산자 평가" $ do
    it "정수에 양수 단항 연산자" $ do
      testExprEval (unaryOp Plus (litInteger 10))
        `shouldEval` (IntegerVal 10, initState)
    it "정수에 음수 단항 연산자" $ do
      testExprEval (unaryOp Minus (litInteger 10))
        `shouldEval` (IntegerVal (-10), initState)
    it "실수에 양수 단항 연산자" $ do
      testExprEval (unaryOp Plus (litReal 10.0))
        `shouldEval` (RealVal 10.0, initState)
    it "실수에 음수 단항 연산자" $ do
      testExprEval (unaryOp Minus (litReal 10.0))
        `shouldEval` (RealVal (-10.0), initState)
    it "함수에 양수 단항 연산자 적용 시 타입 에러" $ do
      testExprEvalWith (unaryOp Minus (var "십")) sampleState
        `shouldEvalError` operateTypeError [FuncType]

  describe "함수 호출 평가" $ do
    it "인자 없는 함수 호출" $ do
      testExprEvalWith (app (var "십") []) sampleState
        `shouldEval` (IntegerVal 10, sampleState)
    it "인자가 하나인 함수 호출" $ do
      testExprEvalWith (app (var "늘리기") [litInteger 10]) sampleState
        `shouldEval` (IntegerVal 20, sampleState)
    it "호출 인수의 개수가 맞지 않으면 에러" $ do
      testExprEvalWith (app (var "늘리기") [litInteger 10, litInteger 20])
                       sampleState
        `shouldEvalError` incorrectArgsNum 1 2
    it "호출할 수 없는 대상에 대해 에러" $ do
      testExprEvalWith (app (var "나이") []) sampleState
        `shouldEvalError` notCallable IntegerType

  describe "대입식 평가" $ do
    it "단순 정수 대입" $ do
      testExprEvalWith (assign "수" (litInteger 10)) sampleState
        `shouldEval` ( IntegerVal 10
                     , over symbolTable
                            (M.insert "수" (IntegerVal 10))
                            sampleState
                     )
    it "사칙연산식 대입" $ do
      testExprEvalWith
          (assign "수" (binaryOp Plus (litInteger 10) (litInteger 10)))
          sampleState
        `shouldEval` ( IntegerVal 20
                     , over symbolTable
                            (M.insert "수" (IntegerVal 20))
                            sampleState
                     )
    it "대입식 대입" $ do
      testExprEvalWith (assign "수" (assign "상자" (litInteger 10))) sampleState
        `shouldEval` ( IntegerVal 10
                     , over
                       symbolTable
                       (M.union
                         (M.fromList
                           [("수", IntegerVal 10), ("상자", IntegerVal 10)]
                         )
                       )
                       sampleState
                     )
    it "겹치는 식별자 대입" $ do
      testExprEvalWith (assign "십" (litInteger 10)) sampleState
        `shouldEval` ( IntegerVal 10
                     , over symbolTable
                            (M.insert "십" (IntegerVal 10))
                            sampleState
                     )
