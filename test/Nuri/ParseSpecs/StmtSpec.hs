module Nuri.ParseSpecs.StmtSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Parse.Stmt
import           Nuri.Util

spec :: Spec
spec = do
  describe "반환 구문 파싱" $ do
    it "단일 정수 반환" $ do
      testParse returnStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "사칙연산식 반환" $ do
      testParse returnStmt "1+2 돌려주다"
        `shouldParse` Return (binaryOp Plus (litInteger 1) (litInteger 2))

  describe "함수 선언문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse functionDecl "[값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        [ ExprStmt $ app "더하다" [var "값", litInteger 1]
                        , Return (var "값")
                        ]
