module Nuri.Spec.Eval.StmtSpec where

import           Test.Hspec

import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Data.Map.Strict

import Nuri.Spec.Util
import Nuri.Spec.Eval.Util

import Nuri.Stmt
import Nuri.Eval.Stmt
import Nuri.Eval.Val
import Nuri.Eval.Flow
import Nuri.Eval.Error

testFlowWith :: Stmt -> SymbolTable -> Either Error (Flow Val Val, SymbolTable)
testFlowWith stmt table =
  runExcept (runStateT (runFlowT (evalStmt stmt False)) table)

testFlow :: Stmt -> Either Error (Flow Val Val, SymbolTable)
testFlow stmt = testFlowWith stmt empty

spec :: Spec
spec = do
  describe "표현식 구문 평가" $ do
    it "Normal 표현식 평가" $ do
      testFlow (ExprStmt $ litInteger 10)
        `shouldEval` (Normal (IntegerVal 10), empty)
  describe "반환 구문 평가" $ do
    it "함수 밖에서 Throw하면 에러" $ do
      testFlow (Return $ litInteger 10) `shouldEvalError` notInFunction
  describe "함수 선언 구문 평가" $ do
    it "인자 없는 함수 선언" $ do
      testFlow (funcDecl "깨우다" [] [Return $ litInteger 10])
        `shouldEval` (Normal Undefined, fromList [("깨우다", funcVal)])
    it "인자가 하나인 함수 선언" $ do
      testFlow (funcDecl "먹다" ["음식"] [Return $ litInteger 10])
        `shouldEval` (Normal Undefined, fromList [("먹다", funcVal)])
    it "함수 이름이 중복되면 에러" $ do
      testFlowWith (funcDecl "십" ["수"] [Return $ litInteger 10]) sampleTable `shouldEvalError` boundSymbol "십"
