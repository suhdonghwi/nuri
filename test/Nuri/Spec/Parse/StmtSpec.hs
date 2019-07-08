module Nuri.Spec.Parse.StmtSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Parse.Stmt

import           Nuri.Spec.Util
import           Nuri.Spec.Parse.Util

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
                        [ ExprStmt $ app (var "더하다") [var "값", litInteger 1]
                        , Return (var "값")
                        ]
    it "함수의 본문이 없으면 에러" $ do
      testParse functionDecl `shouldFailOn` "[값] 증가하다:"

  describe "구문 파싱" $ do
    it "표현식 구문 파싱" $ do
      testParse stmt "1 + 2 줄이다" `shouldParse` ExprStmt
        (binaryOp Plus (litInteger 1) (app (var "줄이다") [litInteger 2]))
    it "반환 구문 파싱" $ do
      testParse stmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "인자가 한 개인 함수" $ do
      testParse stmt "[값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다" `shouldParse` funcDecl
        "증가하다"
        ["값"]
        [ExprStmt $ app (var "더하다") [var "값", litInteger 1], Return (var "값")]
