{-# LANGUAGE OverloadedLists #-}

module Nuri.Spec.Parse.StmtSpec where

import NeatInterpolation
import Nuri.Expr
import Nuri.Parse.Stmt
import Nuri.Spec.Parse.Util
import Nuri.Spec.Util
import Nuri.Stmt
import Test.Hspec

spec :: Spec
spec = do
  describe "선언문 파싱" $ do
    describe "함수 선언문 파싱" $ do
      it "인자가 한 개인 함수" $ do
        testParse parseDeclStmt "함수 [값]을 증가하다:\n  [값]에 1을 더하다"
          `shouldParse` funcDeclStmt
            "증가하다"
            [("값", "을")]
            ( funcCall
                (var "더하다")
                [(var "값", "에"), (litInteger 1, "을")]
            )

      it "인자가 여러 개인 함수" $ do
        testParse parseDeclStmt "동사 [값1]에 [값2]을 더하다:\n   [값1] + [값2]"
          `shouldParse` verbDeclStmt
            "더하다"
            [("값1", "에"), ("값2", "을")]
            (binaryOp Add (var "값1") (var "값2"))

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
          `shouldParse` funcDeclStmt "거짓하다" [("값", "을")] (litInteger 1)

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
            "동작"
            []
            ( Seq
                [ Left $
                    verbDecl
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
            "동작"
            []
            ( Seq
                [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
                  Left $
                    verbDecl
                      "더하다"
                      [("값", "을")]
                      (binaryOp Add (var "값") (litInteger 1)),
                  Left $
                    verbDecl
                      "빼다"
                      [("값", "을")]
                      (binaryOp Subtract (var "값") (litInteger 1)),
                  Right $ litInteger 1
                ]
            )

      it "시퀀스 끝이 선언문인 함수에 대해서 오류" $ do
        testParse
          parseDeclStmt
          `shouldFailOn` ( [text|
              함수 동작: 순서대로
                함수 [값]에 더하다:
                  [값] + 1
                1
                상수 [수]: 10 + 10
            |]
                         )

      it "반의어 1개를 포함한 형용사 선언" $ do
        testParse
          parseDeclStmt
          ( [text|
              형용사 [ㄱ]과 [ㄴ]이 같다 (<-> 다르다):             
                [ㄱ] == [ㄴ]
              |]
          )
          `shouldParse` adjectiveDeclStmt
            "같다"
            [("ㄱ", "와"), ("ㄴ", "이")]
            [Antonym "다르다"]
            (binaryOp Equal (var "ㄱ") (var "ㄴ"))
      it "유의어 1개를 포함한 형용사 선언" $ do
        testParse
          parseDeclStmt
          ( [text|
              형용사 [ㄱ]과 [ㄴ]이 같다 (= 똑같다):             
                [ㄱ] == [ㄴ]
              |]
          )
          `shouldParse` adjectiveDeclStmt
            "같다"
            [("ㄱ", "와"), ("ㄴ", "이")]
            [Synonym "똑같다"]
            (binaryOp Equal (var "ㄱ") (var "ㄴ"))
      it "유의어, 반의어 여러개를 포함한 형용사 선언" $ do
        testParse
          parseDeclStmt
          ( [text|
              형용사 [ㄱ]과 [ㄴ]이 같다 (= 똑같다, <-> 다르다):             
                [ㄱ] == [ㄴ]
              |]
          )
          `shouldParse` adjectiveDeclStmt
            "같다"
            [("ㄱ", "와"), ("ㄴ", "이")]
            [Synonym "똑같다", Antonym "다르다"]
            (binaryOp Equal (var "ㄱ") (var "ㄴ"))
      it "유의어 선언을 포함한 동사 선언에 대해서 오류" $ do
        testParse
          parseDeclStmt
          `shouldFailOn` ( [text|
              동사 [ㄱ]을 먹다 (= 섭취하다):
                [ㄱ] - 1
              |]
                         )
      it "용언의 식별자 조건에 맞지 않는 유의어에 대해서 오류" $ do
        testParse
          parseDeclStmt
          `shouldFailOn` ( [text|
              형용사 [ㄱ]과 [ㄴ]이 같다 (= 동일):
                [ㄱ] - 1
              |]
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

    describe "구조체 선언문 파싱" $ do
      it "필드가 1개인 구조체 선언문" $ do
        testParse parseDeclStmt "구조체 사람: 이름"
          `shouldParse` structDeclStmt "사람" ["이름"]
      it "필드가 3개인 구조체 선언문" $ do
        testParse parseDeclStmt "구조체 사람: 이름, 몸무게, 성별"
          `shouldParse` structDeclStmt "사람" ["이름", "몸무게", "성별"]
      it "필드 이름 중에 공백이 포함되어있는 구조체 선언문" $ do
        testParse parseDeclStmt "구조체 사람: 이름, 몸의 무게, 성별"
          `shouldParse` structDeclStmt "사람" ["이름", "몸의 무게", "성별"]
      it "이름에 공백이 포함되어있는 구조체 선언문에 대해 오류" $ do
        testParse parseDeclStmt `shouldFailOn` "구조체 멋진 사람: 이름, 몸의 무게, 성별"

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
        `shouldParse` [ funcDeclStmt
                          "증가하다"
                          [("값", "에")]
                          ( funcCall
                              (var "더하다")
                              [(var "값", "에"), (litInteger 1, "을")]
                          )
                      ]

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
        `shouldParse` [ funcDeclStmt "더하다" [("값", "에")] (litInteger 1),
                        funcDeclStmt "빼다" [("값", "을")] (litInteger 2)
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
                          "더하다"
                          [("값", "에")]
                          ( Seq
                              [ Right $ funcCall (var "보여주다") [(litInteger 1, "을")],
                                Right $ funcCall (var "보여주다") [(litInteger 3, "을")]
                              ]
                          ),
                        funcDeclStmt
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
            
            10에 가하고 걷다
          |]
        )
        `shouldParse` [ verbDeclStmt "가하다" [("값", "에")] (litInteger 1),
                        verbDeclStmt "걷다" [("값", "을")] (litInteger 2),
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
    it "함수 전방 선언 문법으로 전역 상호재귀 함수 선언 파싱" $ do
      testParse
        parseStmts
        ( [text|
            동사 두번째하다
            동사 첫번째하다: 
              두번째하다

            동사 두번째하다: 
              첫번째하다
            
            첫번째하다
          |]
        )
        `shouldParse` [ verbForwardStmt "두번째하다" [],
                        verbDeclStmt "첫번째하다" [] (funcCall (var "두번째하다") []),
                        verbDeclStmt "두번째하다" [] (funcCall (var "첫번째하다") []),
                        ExprStmt $ funcCall (var "첫번째하다") []
                      ]
    it "함수 전방 선언 문법으로 시퀀스 상호재귀 함수 선언 파싱" $ do
      testParse
        parseStmts
        ( [text|
            순서대로
              동사 두번째하다 
              동사 첫번째하다: 
                두번째하다

              동사 두번째하다: 
                첫번째하다
              
              첫번째하다
          |]
        )
        `shouldParse` [ ExprStmt $
                          Seq
                            [ Left $ verbForward "두번째하다" [],
                              Left $ verbDecl "첫번째하다" [] (funcCall (var "두번째하다") []),
                              Left $ verbDecl "두번째하다" [] (funcCall (var "첫번째하다") []),
                              Right $ funcCall (var "첫번째하다") []
                            ]
                      ]
  describe "품사 테이블 관리" $ do
    it "일반, 형용사, 동사 함수 선언 테이블 추가" $ do
      testParse'
        parseStmts
        ( [text|
            함수 일: 1
            형용사 같다: 2
            동사 달리다: 3
          |]
        )
        `shouldParse'` ( [ funcDeclStmt "일" [] (litInteger 1),
                           adjectiveDeclStmt "같다" [] [] (litInteger 2),
                           verbDeclStmt "달리다" [] (litInteger 3)
                         ],
                         fromList [("일", Normal), ("같다", Adjective), ("달리다", Verb)]
                       )
    it "일반, 형용사, 동사 함수 전방 선언 테이블 추가" $ do
      testParse'
        parseStmts
        ( [text|
            함수 일
            형용사 같다
            동사 달리다
          |]
        )
        `shouldParse'` ( [ funcForwardStmt "일" [],
                           adjectiveForwardStmt "같다" [] [],
                           verbForwardStmt "달리다" []
                         ],
                         fromList [("일", Normal), ("같다", Adjective), ("달리다", Verb)]
                       )
    it "순서대로 표현식 스코프 관리" $ do
      testParse'
        parseStmts
        ( [text|
            함수 일: 1

            순서대로
              함수 이: 2
              이

            형용사 같다: 2
            동사 달리다: 3
          |]
        )
        `shouldParse'` ( [ funcDeclStmt "일" [] (litInteger 1),
                           ExprStmt $
                             Seq
                               [ Left $ funcDecl "이" [] (litInteger 2),
                                 Right $ funcCall (var "이") []
                               ],
                           adjectiveDeclStmt "같다" [] [] (litInteger 2),
                           verbDeclStmt "달리다" [] (litInteger 3)
                         ],
                         fromList [("일", Normal), ("같다", Adjective), ("달리다", Verb)]
                       )
    it "구조체 선언 필드 접근자 테이블 추가" $ do
      testParse'
        parseStmts
        ( [text|
            구조체 사람: 몸의 무게, 나이, 이름
          |]
        )
        `shouldParse'` ( [structDeclStmt "사람" ["몸의 무게", "나이", "이름"]],
                         fromList [("몸의 무게", Normal), ("나이", Normal), ("이름", Normal)]
                       )
    it "상수의 경우 품사 테이블에 추가하지 않음" $ do
      testParse'
        parseStmts
        ( [text|
            함수 일
            상수 [수]: 10
            형용사 같다
            동사 달리다
          |]
        )
        `shouldParse'` ( [ funcForwardStmt "일" [],
                           constDeclStmt "수" (litInteger 10),
                           adjectiveForwardStmt "같다" [] [],
                           verbForwardStmt "달리다" []
                         ],
                         fromList [("일", Normal), ("같다", Adjective), ("달리다", Verb)]
                       )
