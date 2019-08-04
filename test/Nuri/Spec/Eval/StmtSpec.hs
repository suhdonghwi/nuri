module Nuri.Spec.Eval.StmtSpec where

import           Test.Hspec

import qualified Data.Map                      as Map

import           Nuri.Stmt
import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Error

import           Nuri.Spec.Util
import           Nuri.Spec.Eval.Util

testStmtEvalWith
  :: Stmt -> SymbolTable -> IO (Either Error (Flow Val, SymbolTable))
testStmtEvalWith = testEvalWith (`evalStmt` False)

testStmtEval :: Stmt -> IO (Either Error (Flow Val, SymbolTable))
testStmtEval = testEval (`evalStmt` False)

spec :: Spec
spec = do
  describe "표현식 구문 평가" $ do
    it "Normal 표현식이 그대로 평가" $ do
      testStmtEval (ExprStmt (litInteger 10))
        `shouldEval` (Normal (IntegerVal 10), Map.empty)
  describe "반환 구문 평가" $ do
    it "함수 밖에서 Throw하면 에러" $ do
      testStmtEval (Return (litInteger 10)) `shouldEvalError` notInFunction
  describe "조건문 평가" $ do
    it "단일 조건문 평가 (참)" $ do
      testStmtEvalWith
          (ifStmt (litBool True)
                  (ExprStmt (assign "나이" (litInteger 10)))
                  Nothing
          )
          sampleTable
        `shouldEval` ( Normal (IntegerVal 10)
                     , Map.adjust (const (IntegerVal 10)) "나이" sampleTable
                     )
    it "단일 조건문 평가 (거짓)" $ do
      testStmtEvalWith
          (ifStmt (litBool False)
                  (ExprStmt (assign "값" (litInteger 10)))
                  Nothing
          )
          sampleTable
        `shouldEval` (Normal Undefined, sampleTable)
    it "아니면 ~ 조건문 평가" $ do
      testStmtEvalWith
          (ifStmt
            (litBool False)
            (ExprStmt (assign "나이" (litInteger 10)))
            (Just
              (ifStmt (litBool True)
                      (ExprStmt (assign "나이" (litInteger 20)))
                      Nothing
              )
            )
          )
          sampleTable
        `shouldEval` ( Normal (IntegerVal 20)
                     , Map.adjust (const (IntegerVal 20)) "나이" sampleTable
                     )
    it "스코프 적용" $ do
      testStmtEvalWith
          (ifStmt
            (litBool False)
            (ExprStmt (assign "수1" (litInteger 10)))
            (Just
              (ifStmt (litBool True)
                      (ExprStmt (assign "수2" (litInteger 20)))
                      Nothing
              )
            )
          )
          sampleTable
        `shouldEval` (Normal (IntegerVal 20), sampleTable)
  describe "함수 선언 구문 평가" $ do
    it "인자 없는 함수 선언" $ do
      testStmtEval (funcDecl "깨우다" [] (Return (litInteger 10)))
        `shouldEval` (Normal Undefined, Map.fromList [("깨우다", funcVal)])
    it "인자가 하나인 함수 선언" $ do
      testStmtEval (funcDecl "먹다" ["음식"] (Return (litInteger 10)))
        `shouldEval` (Normal Undefined, Map.fromList [("먹다", funcVal)])
    it "함수 이름이 중복되면 에러" $ do
      testStmtEvalWith (funcDecl "십" ["수"] (Return (litInteger 10))) sampleTable
        `shouldEvalError` boundSymbol "십"
