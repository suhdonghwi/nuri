{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Parse.StmtSpec where

import           NeatInterpolation
import           Data.Text                                ( unpack )

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Expr
import           Nuri.Stmt
import           Nuri.Parse.Stmt

import           Nuri.Spec.Util
import           Nuri.Spec.Parse.Util


spec :: Spec
spec = do
  describe "선언문 파싱" $ do
    describe "함수 선언문 파싱" $ do
      it "인자가 한 개인 함수" $ do
        testParse parseDeclStmt "함수 [값]을 증가하다:\n  [값]에 1을 더하다"
          `shouldParse` funcDecl
                          "증가하다"
                          [("값", "을")]
                          (funcCall (var "더하다")
                                    [(var "값", "에"), (litInteger 1, "을")]
                          )

      it "인자가 여러 개인 함수" $ do
        testParse parseDeclStmt "함수 [값1]에 [값2]을 더하다:\n   [값1] + [값2]"
          `shouldParse` funcDecl "더하다"
                                 [("값1", "에"), ("값2", "을")]
                                 (binaryOp Add (var "값1") (var "값2"))

      it "함수 이름에 띄어쓰기가 포함된 함수" $ do
        testParse parseDeclStmt "함수 [값1]에 [값2]을 더한 값 구하다:\n  참"
          `shouldParse` funcDecl
                          "더한 값 구하다"
                          [("값1", "에"), ("값2", "을")]
                          (litBool True)

      it "함수의 본문이 없으면 오류" $ do
        testParse parseDeclStmt `shouldFailOn` "함수 [값]을 증가하다:"

      it "함수의 이름이 예약어면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` (unpack [text|
              함수 [값]을 거짓:
                1 반환하다
            |]
                         )
      it "인자에 대한 조사가 없으면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` (unpack [text|
              함수 [값] 더하다:
                1 반환하다
            |]
                         )
        testParse parseDeclStmt
          `shouldFailOn` (unpack [text|
              함수 [수1]에 [값] 더하다:
                1 반환하다
            |]
                         )
      it "예약어로 시작하는 이름의 함수" $ do
        testParse parseDeclStmt "함수 [값]을 거짓하다:\n  1"
          `shouldParse` funcDecl "거짓하다" [("값", "을")] (litInteger 1)

      it "연산식 시퀀스를 포함한 함수" $ do
        testParse
            parseDeclStmt
            (unpack [text| 
              함수 동작:
                1 + 1 
                2 * 2
            |]
            )
          `shouldParse` funcDecl
                          "동작"
                          []
                          (Seq
                            [ binaryOp Add      (litInteger 1) (litInteger 1)
                            , binaryOp Multiply (litInteger 2) (litInteger 2)
                            ]
                          )

      it "중간에 비어있는 라인을 포함한 시퀀스를 가진 함수" $ do
        testParse
            parseDeclStmt
            (unpack [text|
              함수 동작:
                1
                                            
                1을 던지다
            |]
            )
          `shouldParse` funcDecl
                          "동작"
                          []
                          (Seq
                            [ litInteger 1
                            , funcCall (var "던지다") [(litInteger 1, "을")]
                            ]
                          )

      it "시퀀스 중간에 함수 선언을 하는 함수" $ do
        testParse
            parseDeclStmt
            (unpack [text|
              함수 동작:
                함수 [값]을 더하다:
                  [값] + 1
                1
            |]
            )
          `shouldParse` funcDecl
                          "동작"
                          []
                          (letExpr
                            "더하다"
                            (lambda [("값", "을")]
                                    (binaryOp Add (var "값") (litInteger 1))
                            )
                            (litInteger 1)
                          )

      it "시퀀스 중간에 함수를 2개 선언하는 함수" $ do
        testParse
            parseDeclStmt
            (unpack [text|
              함수 동작:
                1 + 1
                함수 [값]을 더하다:
                  [값] + 1

                함수 [값]을 빼다:
                  [값] - 1

                1
            |]
            )
          `shouldParse` funcDecl
                          "동작"
                          []
                          (Seq
                            [ binaryOp Add (litInteger 1) (litInteger 1)
                            , letExpr
                              "더하다"
                              (lambda [("값", "을")]
                                      (binaryOp Add (var "값") (litInteger 1))
                              )
                              (letExpr
                                "빼다"
                                (lambda
                                  [("값", "을")]
                                  (binaryOp Subtract (var "값") (litInteger 1))
                                )
                                (litInteger 1)
                              )
                            ]
                          )

      it "시퀀스 끝이 선언문인 함수" $ do
        testParse
            parseDeclStmt
            (unpack [text|
              함수 동작:
                함수 [값]에 더하다:
                  [값] + 1
                1
                상수 [수]: 10 + 10
            |]
            )
          `shouldParse` funcDecl
                          "동작"
                          []
                          (letExpr
                            "더하다"
                            (lambda [("값", "에")]
                                    (binaryOp Add (var "값") (litInteger 1))
                            )
                            (Seq
                              [ (litInteger 1)
                              , binaryOp Add (litInteger 10) (litInteger 10)
                              ]
                            )
                          )


    describe "상수 선언문 파싱" $ do
      it "단순 리터럴 상수 선언" $ do
        testParse parseDeclStmt "상수 [값]: 1"
          `shouldParse` constDecl "값" (litInteger 1)
      it "계산식 상수 선언" $ do
        testParse parseDeclStmt "상수 [값] : 1 * 2"
          `shouldParse` constDecl
                          "값"
                          (binaryOp Multiply (litInteger 1) (litInteger 2))

  describe "표현식 구문 파싱" $ do
    it "계산식 구문" $ do
      testParse parseExprStmt "1 + 2 * 3" `shouldParse` ExprStmt
        (binaryOp Add
                  (litInteger 1)
                  (binaryOp Multiply (litInteger 2) (litInteger 3))
        )

  describe "구문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse parseStmt "함수 [값]에 증가하다:\n  [값]에 1을 더하다" `shouldParse` funcDecl
        "증가하다"
        [("값", "에")]
        (funcCall (var "더하다") [(var "값", "에"), (litInteger 1, "을")])

  describe "구문 여러 개 파싱" $ do
    it "함수 여러 개 선언 파싱" $ do
      testParse
          parseStmts
          (unpack [text|
            함수 [값]에 더하다: 
              1

            함수 [값]을 빼다: 
              2
          |]
          )
        `shouldParse` [ funcDecl "더하다" [("값", "에")] (litInteger 1)
                      , funcDecl "빼다"  [("값", "을")] (litInteger 2)
                      ]
    it "시퀀스를 포함한 함수 여러 개 선언 파싱" $ do
      testParse
          parseStmts
          (unpack [text|
            함수 [값]에 더하다: 
              1을 보여주다
              3을 보여주다
            함수 [값]을 빼다:
                1을 보여주다
                2 + 3
          |]
          )
        `shouldParse` [ funcDecl
                        "더하다"
                        [("값", "에")]
                        (Seq
                          [ funcCall (var "보여주다") [(litInteger 1, "을")]
                          , funcCall (var "보여주다") [(litInteger 3, "을")]
                          ]
                        )
                      , funcDecl
                        "빼다"
                        [("값", "을")]
                        (Seq
                          [ funcCall (var "보여주다") [(litInteger 1, "을")]
                          , binaryOp Add (litInteger 2) (litInteger 3)
                          ]
                        )
                      ]

