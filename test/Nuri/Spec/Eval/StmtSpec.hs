{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Eval.StmtSpec where

import           Test.Hspec

import           Control.Lens                      hiding ( assign )

import qualified Data.Map                      as M

import           Nuri.Stmt
import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Error

import           Nuri.Spec.Util
import           Nuri.Spec.Eval.Util

testStmtEvalWith
  :: Stmt -> InterpreterState -> IO (Either Error (Flow Val, InterpreterState))
testStmtEvalWith = testEvalWith evalStmt

testStmtEval :: Stmt -> IO (Either Error (Flow Val, InterpreterState))
testStmtEval = testEval evalStmt

spec :: Spec
spec = do
  describe "표현식 구문 평가" $ do
    it "Normal 표현식이 그대로 평가" $ do
      testStmtEval (ExprStmt (litInteger 10))
        `shouldEval` (Normal (IntegerVal 10), initState)
  describe "반환 구문 평가" $ do
    it "함수 밖에서 Throw하면 에러" $ do
      testStmtEval (Return (litInteger 10)) `shouldEvalError` notInFunction
  describe "조건문 평가" $ do
    it "단일 조건문 평가 (참)" $ do
      testStmtEvalWith
          (ifStmt (litBool True)
                  [ExprStmt (assign "나이" (litInteger 10))]
                  Nothing
          )
          initState
        `shouldEval` ( Normal (IntegerVal 10)
                     , over symbolTable
                            (M.adjust (const (IntegerVal 10)) "나이")
                            initState
                     )
    it "단일 조건문 평가 (거짓)" $ do
      testStmtEvalWith
          (ifStmt (litBool False)
                  [ExprStmt (assign "값" (litInteger 10))]
                  Nothing
          )
          initState
        `shouldEval` (Normal Undefined, initState)
    it "아니면 ~ 조건문 평가" $ do
      testStmtEvalWith
          (ifStmt
            (litBool False)
            [ExprStmt (assign "나이" (litInteger 10))]
            (Just
              [ ifStmt (litBool True)
                       [ExprStmt (assign "나이" (litInteger 20))]
                       Nothing
              ]
            )
          )
          initState
        `shouldEval` ( Normal (IntegerVal 20)
                     , over symbolTable
                            (M.adjust (const (IntegerVal 20)) "나이")
                            initState
                     )
    it "스코프 적용" $ do
      testStmtEvalWith
          (ifStmt
            (litBool False)
            [ExprStmt (assign "수1" (litInteger 10))]
            (Just
              [ ifStmt (litBool True)
                       [ExprStmt (assign "수2" (litInteger 20))]
                       Nothing
              ]
            )
          )
          initState
        `shouldEval` (Normal (IntegerVal 20), initState)
  describe "함수 선언 구문 평가" $ do
    it "인자 없는 함수 선언" $ do
      testStmtEval (funcDecl "깨우다" [] [Return (litInteger 10)])
        `shouldEval` ( Normal Undefined
                     , set symbolTable (M.fromList [("깨우다", funcVal)]) initState
                     )
    it "인자가 하나인 함수 선언" $ do
      testStmtEval (funcDecl "먹다" ["음식"] [Return (litInteger 10)])
        `shouldEval` ( Normal Undefined
                     , set symbolTable (M.fromList [("먹다", funcVal)]) initState
                     )
    it "함수 이름이 중복되면 에러" $ do
      testStmtEvalWith (funcDecl "십" ["수"] [Return (litInteger 10)]) sampleState
        `shouldEvalError` boundSymbol "십"
