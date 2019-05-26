module Nuri.ParseSpecs.StmtSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Text.Megaparsec

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Parse.Stmt
import           Nuri.ParseSpecs.Util

spec :: Spec
spec = do
  describe "반환 구문 파싱" $ do
    it "단일 정수 반환" $ do
      testParse returnStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "사칙연산식 반환" $ do
      testParse returnStmt "1+2 돌려주다"
        `shouldParse` Return (binaryOp Plus (litInteger 1) (litInteger 2))
