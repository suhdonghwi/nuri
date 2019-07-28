module Nuri.Spec.Eval.StmtSpec where

import           Test.Hspec

import qualified Data.Map                      as Map
import           Data.List.NonEmpty

import           Nuri.Spec.Util
import           Nuri.Spec.Eval.Util

import           Nuri.Stmt
import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Flow
import           Nuri.Eval.Error

testFlowWith
  :: Stmt -> SymbolTable -> IO (Either Error (Flow Val Val, SymbolTable))
testFlowWith = runStmtEval

testFlow :: Stmt -> IO (Either Error (Flow Val Val, SymbolTable))
testFlow stmt = testFlowWith stmt Map.empty

spec :: Spec
spec = do
  describe "표현식 구문 평가" $ do
    it "Normal 표현식 평가" $ do
      testFlow (ExprStmt (litInteger 10))
        `shouldEval` (Normal (IntegerVal 10), Map.empty)
  describe "반환 구문 평가" $ do
    it "함수 밖에서 Throw하면 에러" $ do
      testFlow (Return (litInteger 10)) `shouldEvalError` notInFunction
  describe "조건문 평가" $ do
    it "단일 조건문 평가 (참)" $ do
      testFlow
          (ifStmt
            ((litBool True, [ExprStmt (assign "값" (litInteger 10))]) :| [])
            Nothing
          )
        `shouldEval` ( Normal (IntegerVal 10)
                     , Map.fromList [("값", IntegerVal 10)]
                     )
    it "단일 조건문 평가 (거짓)" $ do
      testFlow
          (ifStmt
            ((litBool False, [ExprStmt (assign "값" (litInteger 10))]) :| [])
            Nothing
          )
        `shouldEval` (Normal Undefined, Map.empty)
    it "아니고 ~ 이면 조건문 평가" $ do
      testFlow
          (ifStmt
            (  (litBool False, [ExprStmt (assign "값" (litInteger 10))])
            :| [(litBool True, [ExprStmt (assign "값2" (litInteger 20))])]
            )
            Nothing
          )
        `shouldEval` ( Normal (IntegerVal 20)
                     , Map.fromList [("값2", IntegerVal 20)]
                     )
    it "아니면 ~ 조건문 평가" $ do
      testFlow
          (ifStmt
            (  (litBool False, [ExprStmt (assign "값" (litInteger 10))])
            :| [(litBool False, [ExprStmt (assign "값2" (litInteger 20))])]
            )
            (Just [ExprStmt (assign "값3" (litInteger 30))])
          )
        `shouldEval` ( Normal (IntegerVal 30)
                     , Map.fromList [("값3", IntegerVal 30)]
                     )
  describe "함수 선언 구문 평가" $ do
    it "인자 없는 함수 선언" $ do
      testFlow (funcDecl "깨우다" [] [Return (litInteger 10)])
        `shouldEval` (Normal Undefined, Map.fromList [("깨우다", funcVal)])
    it "인자가 하나인 함수 선언" $ do
      testFlow (funcDecl "먹다" ["음식"] [Return (litInteger 10)])
        `shouldEval` (Normal Undefined, Map.fromList [("먹다", funcVal)])
    it "함수 이름이 중복되면 에러" $ do
      testFlowWith (funcDecl "십" ["수"] [Return (litInteger 10)]) sampleTable
        `shouldEvalError` boundSymbol "십"
