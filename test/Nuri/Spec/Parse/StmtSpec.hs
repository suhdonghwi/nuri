{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Parse.StmtSpec where

import           NeatInterpolation
import           Data.Text                                ( unpack )

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Expr
import           Nuri.Parse.Stmt

import           Nuri.Spec.Util
import           Nuri.Spec.Parse.Util

spec :: Spec
spec = do
  describe "함수 선언문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse parseFuncDecl "함수 [값] 증가하다: [값] 1 더하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        (funcCall "더하다" [var "값", litInteger 1])

    it "인자가 여러 개인 함수" $ do
      testParse parseFuncDecl "함수 [값1] [값2] 더하다: [값1] + [값2]"
        `shouldParse` funcDecl
                        "더하다"
                        ["값1", "값2"]
                        (binaryOp Add (var "값1") (var "값2"))

    it "함수 이름에 띄어쓰기가 포함된 함수" $ do
      testParse parseFuncDecl "함수 [값1] [값2] 피보나치 구하다: 참"
        `shouldParse` funcDecl "피보나치 구하다" ["값1", "값2"] (litBool True)

    -- it "함수 속의 함수" $ do
    --   testParse parseFuncDecl "함수 [값] 더하다:\n  함수 [값2] 빼다:\n    1 반환하다\n  2 반환하다"
    --     `shouldParse` funcDecl
    --                     "더하다"
    --                     ["값"]
    --                     [ funcDecl "빼다" ["값2"] [Return (litInteger 1)]
    --                     , Return (litInteger 2)
    --                     ]

    it "함수의 본문이 없으면 에러" $ do
      testParse parseFuncDecl `shouldFailOn` "함수 [값] 증가하다:"
    it "함수의 이름이 예약어면 에러" $ do
      testParse parseFuncDecl
        `shouldFailOn` (unpack [text|
            함수 [값] 거짓:
              1 반환하다
          |]
                       )
    it "예약어로 시작하는 이름의 함수" $ do
      testParse parseFuncDecl "함수 [값] 거짓하다: 1"
        `shouldParse` funcDecl "거짓하다" ["값"] (litInteger 1)

  describe "구문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse parseStmt "함수 [값] 증가하다: [값] 1 더하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        (funcCall "더하다" [var "값", litInteger 1])

  describe "구문 여러 개 파싱" $ do
    it "함수 여러 개 선언 파싱" $ do
      testParse
          parseStmts
          (unpack [text|
            함수 [값] 더하다: 1
            함수 [값] 빼다: 
              2
          |]
          )
        `shouldParse` [ funcDecl "더하다" ["값"] (litInteger 1)
                      , funcDecl "빼다"  ["값"] (litInteger 2)
                      ]
    it "시퀀스를 포함한 함수 여러 개 선언 파싱" $ do
      testParse
          parseStmts
          (unpack [text|
            함수 [값] 더하다: 순서대로
              1 보여주다
              3 보여주다

            함수 [값] 빼다:
              순서대로
                1 보여주다
                2 + 3
          |]
          )
        `shouldParse` [ funcDecl
                        "더하다"
                        ["값"]
                        (Seq
                          [ funcCall "보여주다" [litInteger 1]
                          , funcCall "보여주다" [litInteger 3]
                          ]
                        )
                      , funcDecl
                        "빼다"
                        ["값"]
                        (Seq
                          [ funcCall "보여주다" [litInteger 1]
                          , binaryOp Add (litInteger 2) (litInteger 3)
                          ]
                        )
                      ]

