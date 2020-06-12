{-# LANGUAGE OverloadedLists #-}

module Nuri.Spec.Parse.StmtSpec where

import NeatInterpolation
import Nuri.Expr
import Nuri.Parse.Stmt
import Nuri.Spec.Parse.Util
import Nuri.Spec.Util
import Nuri.Stmt
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
  describe "선언문 파싱" $ do
    describe "함수 선언문 파싱" $ do
      it "인자가 한 개인 함수" $ do
        testParse parseDeclStmt "함수 [값]을 증가하다:\n  [값]에 1을 더하다"
          `shouldParse` funcDeclStmt
            NormalDecl
            "증가하다"
            [("값", "을")]
            ( funcCall
                (var "더하다")
                [(var "값", "에"), (litInteger 1, "을")]
            )

      it "인자가 여러 개인 함수" $ do
        testParse parseDeclStmt "동사 [값1]에 [값2]을 더하다:\n   [값1] + [값2]"
          `shouldParse` funcDeclStmt
            VerbDecl
            "더하다"
            [("값1", "에"), ("값2", "을")]
            (binaryOp Add (var "값1") (var "값2"))

      it "함수 이름에 띄어쓰기가 포함된 함수" $ do
        testParse parseDeclStmt "함수 [값1]에 [값2]을 더한 값 구하다:\n  참"
          `shouldParse` funcDeclStmt
            NormalDecl
            "더한 값 구하다"
            [("값1", "에"), ("값2", "을")]
            (litBool True)

      it "함수의 본문이 없으면 오류" $ do
        testParse parseDeclStmt `shouldFailOn` "함수 [값]을 증가하다:"

      it "함수의 이름이 예약어면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              함수 [값]을 거짓:
                1 반환하다
            |]
                         )

      it "용언 함수의 이름이 ~(하)다 꼴이 아니면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              동사 [값]을 보기:
                1 반환하다
            |]
                         )
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              형용사 [값]을 화기:
                1 반환하다
            |]
                         )

      it "인자에 대한 조사가 없으면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              함수 [값] 더하다:
                1 반환하다
            |]
                         )
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              함수 [수1]에 [값] 더하다:
                1 반환하다
            |]
                         )
      it "조사를 중복되게 사용하면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              함수 [값1]을 [값2]를 더하다:
                1 반환하다
            |]
                         )
      it "함수 인자의 이름을 중복되게 사용하면 오류" $ do
        testParse parseDeclStmt
          `shouldFailOn` ( [text|
              함수 [값1]을 [값1]과 더하다:
                1 반환하다
            |]
                         )
      it "예약어로 시작하는 이름의 함수 허용" $ do
        testParse parseDeclStmt "함수 [값]을 거짓하다:\n  1"
          `shouldParse` funcDeclStmt NormalDecl "거짓하다" [("값", "을")] (litInteger 1)

      it "연산식 시퀀스를 포함한 함수" $ do
        testParse
          parseDeclStmt
          ( [text| 
              함수 동작: 순서대로
                1 + 1 
                2 * 2
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
                  Right $
                    binaryOp Multiply (litInteger 2) (litInteger 2)
                ]
            )

      it "비어있는 라인을 포함한 시퀀스를 가진 함수" $ do
        testParse
          parseDeclStmt
          ( [text|
              함수 동작: 순서대로

                1
                1을 던지다
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Right $ litInteger 1,
                  Right $ funcCall (var "던지다") [(litInteger 1, "을")]
                ]
            )

        testParse
          parseDeclStmt
          ( [text|
              함수 동작: 순서대로
                1
                                            
                1을 던지다
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Right $ litInteger 1,
                  Right $ funcCall (var "던지다") [(litInteger 1, "을")]
                ]
            )

      it "시퀀스 중간에 함수 선언을 하는 함수" $ do
        testParse
          parseDeclStmt
          ( [text|
              함수 동작: 순서대로
                동사 [값]을 더하다:
                  [값] + 1
                1
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Left $
                    funcDecl
                      VerbDecl
                      "더하다"
                      [("값", "을")]
                      (binaryOp Add (var "값") (litInteger 1)),
                  Right $ litInteger 1
                ]
            )

      it "시퀀스 중간에 함수를 2개 선언하는 함수" $ do
        testParse
          parseDeclStmt
          ( [text|
              함수 동작: 순서대로
                1 + 1
                동사 [값]을 더하다:
                  [값] + 1

                동사 [값]을 빼다:
                  [값] - 1

                1
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
                  Left $
                    funcDecl
                      VerbDecl
                      "더하다"
                      [("값", "을")]
                      (binaryOp Add (var "값") (litInteger 1)),
                  Left $
                    funcDecl
                      VerbDecl
                      "빼다"
                      [("값", "을")]
                      (binaryOp Subtract (var "값") (litInteger 1)),
                  Right $ litInteger 1
                ]
            )

      it "시퀀스 끝이 선언문인 함수" $ do
        testParse
          parseDeclStmt
          ( [text|
              함수 동작: 순서대로
                함수 [값]에 더하다:
                  [값] + 1
                1
                상수 [수]: 10 + 10
            |]
          )
          `shouldParse` funcDeclStmt
            NormalDecl
            "동작"
            []
            ( Seq
                [ Left $
                    funcDecl
                      NormalDecl
                      "더하다"
                      [("값", "에")]
                      (binaryOp Add (var "값") (litInteger 1)),
                  Right $ litInteger 1,
                  Left $
                    constDecl
                      "수"
                      (binaryOp Add (litInteger 10) (litInteger 10))
                ]
            )

    describe "상수 선언문 파싱" $ do
      it "단순 리터럴 상수 선언" $ do
        testParse parseDeclStmt "상수 [값]: 1"
          `shouldParse` constDeclStmt "값" (litInteger 1)
      it "계산식 상수 선언" $ do
        testParse parseDeclStmt "상수 [값]: 1 * 2"
          `shouldParse` constDeclStmt
            "값"
            (binaryOp Multiply (litInteger 1) (litInteger 2))

  describe "표현식 구문 파싱" $ do
    it "계산식 구문" $ do
      testParse parseExprStmt "1 + 2 * 3"
        `shouldParse` ExprStmt
          ( binaryOp
              Add
              (litInteger 1)
              (binaryOp Multiply (litInteger 2) (litInteger 3))
          )

  describe "구문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse parseStmt "함수 [값]에 증가하다:\n  [값]에 1을 더하다"
        `shouldParse` funcDeclStmt
          NormalDecl
          "증가하다"
          [("값", "에")]
          ( funcCall
              (var "더하다")
              [(var "값", "에"), (litInteger 1, "을")]
          )

  describe "구문 여러 개 파싱" $ do
    it "함수 여러 개 선언 파싱" $ do
      testParse
        parseStmts
        ( [text|
            함수 [값]에 더하다: 
              1

            함수 [값]을 빼다: 
              2
          |]
        )
        `shouldParse` [ funcDeclStmt NormalDecl "더하다" [("값", "에")] (litInteger 1),
                        funcDeclStmt NormalDecl "빼다" [("값", "을")] (litInteger 2)
                      ]
    it "시퀀스를 포함한 함수 여러 개 선언 파싱" $ do
      testParse
        parseStmts
        ( [text|
            함수 [값]에 더하다: 순서대로
              1을 보여주다
              3을 보여주다

            함수 [값]을 빼다: 순서대로
                1을 보여주다
                2 + 3
          |]
        )
        `shouldParse` [ funcDeclStmt
                          NormalDecl
                          "더하다"
                          [("값", "에")]
                          ( Seq
                              [ Right $ funcCall (var "보여주다") [(litInteger 1, "을")],
                                Right $ funcCall (var "보여주다") [(litInteger 3, "을")]
                              ]
                          ),
                        funcDeclStmt
                          NormalDecl
                          "빼다"
                          [("값", "을")]
                          ( Seq
                              [ Right $ funcCall (var "보여주다") [(litInteger 1, "을")],
                                Right $ binaryOp Add (litInteger 2) (litInteger 3)
                              ]
                          )
                      ]
    it "동사 여러 개 선언 뒤 '~(하)고' 용언 활용 파싱" $ do
      testParse
        parseStmts
        ( [text|
            동사 [값]에 가하다: 
              1

            동사 [값]을 걷다: 
              2
            
            10에 가하고, 걷다
          |]
        )
        `shouldParse` [ funcDeclStmt VerbDecl "가하다" [("값", "에")] (litInteger 1),
                        funcDeclStmt VerbDecl "걷다" [("값", "을")] (litInteger 2),
                        ExprStmt $ funcCall (var "걷다") [(funcCall (var "가하다") [(litInteger 10, "에")], "_")]
                      ]
    it "시퀀스에서 함수 선언 뒤 외부 스코프에서 활용하면 에러" $ do
      testParse
        parseStmts
        `shouldFailOn` ( [text|
            순서대로
              동사 [값]에 가하다: 
                1
              1

            동사 [값]을 걷다: 
              2
            
            10에 가하고, 걷다
          |]
                       )