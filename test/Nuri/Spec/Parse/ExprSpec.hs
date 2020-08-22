{-# LANGUAGE OverloadedLists #-}

module Nuri.Spec.Parse.ExprSpec where

import NeatInterpolation
import Nuri.Expr
import Nuri.Parse.Expr
import Nuri.Parse.Term
import Nuri.Parse.Util
import Nuri.Spec.Parse.Util
import Nuri.Spec.Util
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "없음(None) 파싱" $ do
    it "없음을 없음으로 파싱" $ do
      testParse parseNoneExpr "없음" `shouldParse` litNone

  describe "정수 파싱" $ do
    describe "2진수 파싱" $ do
      it "0b1010을 10으로 파싱" $ do
        testParse parseIntegerExpr "0b1010" `shouldParse` litInteger 10
      it "0B1010을 10으로 파싱" $ do
        testParse parseIntegerExpr "0B1010" `shouldParse` litInteger 10
      it "0b0000을 0으로 파싱" $ do
        testParse parseIntegerExpr "0b0000" `shouldParse` litInteger 0

    describe "8진수 파싱" $ do
      it "012을 10으로 파싱" $ do
        testParse parseIntegerExpr "012" `shouldParse` litInteger 10
      it "000을 0으로 파싱" $ do
        testParse parseIntegerExpr "000" `shouldParse` litInteger 0

    describe "10진수 파싱" $ do
      it "10을 10으로 파싱" $ do
        testParse parseIntegerExpr "10" `shouldParse` litInteger 10
      it "0을 0으로 파싱" $ do
        testParse parseIntegerExpr "0" `shouldParse` litInteger 0

    describe "16진수 파싱" $ do
      it "0x000A를 10으로 파싱" $ do
        testParse parseIntegerExpr "0x000A" `shouldParse` litInteger 10
      it "0x0010을 16으로 파싱" $ do
        testParse parseIntegerExpr "0x0010" `shouldParse` litInteger 16
      it "0xffFF를 65535으로 파싱" $ do
        testParse parseIntegerExpr "0xffFF" `shouldParse` litInteger 65535
      it "0XffFF를 65535으로 파싱" $ do
        testParse parseIntegerExpr "0XffFF" `shouldParse` litInteger 65535
      it "0x0000을 0으로 파싱" $ do
        testParse parseIntegerExpr "0x0000" `shouldParse` litInteger 0

  describe "실수 파싱" $ do
    it "0.1을 0.1로 파싱" $ do
      testParse parseReal "0.1" `shouldParse` 0.1
    it "0.01을 0.01로 파싱" $ do
      testParse parseReal "0.01" `shouldParse` 0.01
    it "10.0을 10.0으로 파싱" $ do
      testParse parseReal "10.0" `shouldParse` 10.0
    it "0을 0.0으로 파싱" $ do
      testParse parseReal "0.0" `shouldParse` 0.0

  describe "문자 파싱" $ do
    it "'a'를 'a'로 파싱" $ do
      testParse parseChar "'a'" `shouldParse` 'a'
    it "'가'를 '가'로 파싱" $ do
      testParse parseChar "'가'" `shouldParse` '가'
    it "'\\''를 '\\''로 파싱" $ do
      testParse parseChar "'\\''" `shouldParse` '\''
    it "'\\n를 '\\n로 파싱" $ do
      testParse parseChar "'\\n'" `shouldParse` '\n'
    it "'ab'에 대해서 오류" $ do
      testParse (parseChar *> eof) `shouldFailOn` "'ab'"
    it "'''에 대해서 오류" $ do
      testParse (parseChar *> eof) `shouldFailOn` "'''"

  describe "부울 파싱" $ do
    it "참을 True로 파싱" $ do
      testParse parseBool "참" `shouldParse` True
    it "거짓을 False로 파싱" $ do
      testParse parseBool "거짓" `shouldParse` False

  describe "사칙연산식 파싱" $ do
    describe "단항 연산자" $ do
      it "양의 부호 정수" $ do
        testParse parseArithmetic "+2"
          `shouldParse` unaryOp Positive (litInteger 2)
      it "(떨어져 있는) 양의 부호 정수" $ do
        testParse parseArithmetic "+ 2"
          `shouldParse` unaryOp Positive (litInteger 2)
      it "음의 부호 정수" $ do
        testParse parseArithmetic "-2"
          `shouldParse` unaryOp Negative (litInteger 2)
      it "(떨어져 있는) 음의 부호 정수" $ do
        testParse parseArithmetic "- 2"
          `shouldParse` unaryOp Negative (litInteger 2)
      it "양의 부호 실수" $ do
        testParse parseArithmetic "+2.5"
          `shouldParse` unaryOp Positive (litReal 2.5)
      it "음의 부호 실수" $ do
        testParse parseArithmetic "-2.5"
          `shouldParse` unaryOp Negative (litReal 2.5)
      it "논리 부정 부울값" $ do
        testParse parseArithmetic "!참"
          `shouldParse` unaryOp LogicNot (litBool True)
    describe "이항 연산자" $ do
      it "두 정수 더하기" $ do
        testParse parseArithmetic "1 + 2"
          `shouldParse` binaryOp Add (litInteger 1) (litInteger 2)
      it "두 실수 더하기" $ do
        testParse parseArithmetic "1.0 + 2.5"
          `shouldParse` binaryOp Add (litReal 1.0) (litReal 2.5)
      it "실수와 정수 더하기" $ do
        testParse parseArithmetic "1.0 + 2"
          `shouldParse` binaryOp Add (litReal 1.0) (litInteger 2)
      it "(붙어있는) 두 정수 더하기" $ do
        testParse parseArithmetic "1+2"
          `shouldParse` binaryOp Add (litInteger 1) (litInteger 2)
      it "(부호있는) 두 정수 더하기" $ do
        testParse parseArithmetic "1++2"
          `shouldParse` binaryOp
            Add
            (litInteger 1)
            (unaryOp Positive (litInteger 2))
      it "두 정수 빼기" $ do
        testParse parseArithmetic "2 - 4"
          `shouldParse` binaryOp Subtract (litInteger 2) (litInteger 4)
      it "두 정수 곱하기" $ do
        testParse parseArithmetic "2 * 4"
          `shouldParse` binaryOp Multiply (litInteger 2) (litInteger 4)
      it "두 정수 나누기" $ do
        testParse parseArithmetic "8 / 2"
          `shouldParse` binaryOp Divide (litInteger 8) (litInteger 2)
      it "두 정수 동등 비교" $ do
        testParse parseArithmetic "8 == 2"
          `shouldParse` binaryOp Equal (litInteger 8) (litInteger 2)
      it "두 정수 부등 비교" $ do
        testParse parseArithmetic "8 != 2"
          `shouldParse` binaryOp Inequal (litInteger 8) (litInteger 2)
      it "두 정수 대소 비교 (LT)" $ do
        testParse parseArithmetic "8 < 2"
          `shouldParse` binaryOp LessThan (litInteger 8) (litInteger 2)
      it "두 정수 대소 비교 (GT)" $ do
        testParse parseArithmetic "8 > 2"
          `shouldParse` binaryOp GreaterThan (litInteger 8) (litInteger 2)
      it "두 정수 대소 비교 (LTE)" $ do
        testParse parseArithmetic "8 <= 2"
          `shouldParse` binaryOp LessThanEqual (litInteger 8) (litInteger 2)
      it "두 정수 대소 비교 (GTE)" $ do
        testParse parseArithmetic "8 >= 2"
          `shouldParse` binaryOp GreaterThanEqual (litInteger 8) (litInteger 2)
      it "두 부울값 '그리고' 연산" $ do
        testParse parseArithmetic "참 그리고 거짓"
          `shouldParse` binaryOp LogicAnd (litBool True) (litBool False)
      it "두 부울값 '또는' 연산" $ do
        testParse parseArithmetic "참 또는 거짓"
          `shouldParse` binaryOp LogicOr (litBool True) (litBool False)
    describe "복합 연산" $ do
      it "두 정수 나누고 한 정수 더하기" $ do
        testParse parseArithmetic "4 / 2 + 3"
          `shouldParse` binaryOp
            Add
            (binaryOp Divide (litInteger 4) (litInteger 2))
            (litInteger 3)
      it "두 정수 나누고 한 정수 더하기 (순서 바꿔서)" $ do
        testParse parseArithmetic "3 + 4 / 2"
          `shouldParse` binaryOp
            Add
            (litInteger 3)
            (binaryOp Divide (litInteger 4) (litInteger 2))

  describe "식별자 파싱" $ do
    it "영문 식별자" $ do
      testParse parseIdentifier "[foo]" `shouldParse` "foo"
    it "(대문자) 영문 식별자" $ do
      testParse parseIdentifier "[Foo]" `shouldParse` "Foo"
    it "한글 음절 식별자" $ do
      testParse parseIdentifier "[사과]" `shouldParse` "사과"
    it "한글 자음 식별자" $ do
      testParse parseIdentifier "[ㅅㄷㅎ]" `shouldParse` "ㅅㄷㅎ"
    it "한글 모음 식별자" $ do
      testParse parseIdentifier "[ㅓㅗㅜㅣ]" `shouldParse` "ㅓㅗㅜㅣ"
    it "숫자가 포함된 식별자" $ do
      testParse parseIdentifier "[사람2]" `shouldParse` "사람2"
    it "공백이 포함된 식별자" $ do
      testParse parseIdentifier "[사과는 맛있다]" `shouldParse` "사과는 맛있다"
    it "숫자로 시작하는 식별자에 대해 오류" $ do
      testParse parseIdentifier `shouldFailOn` "[10마리 펭귄]"
    it "공백으로 시작하는 식별자에 대해 오류" $ do
      testParse parseIdentifier `shouldFailOn` "[ Foo]"
    -- it "공백으로 끝나는 식별자는 트리밍" $ do
    --   testParse parseIdentifierExpr "[Foo ]" `shouldParse` var "Foo"
    it "공백만 포함된 식별자에 대해서 오류" $ do
      testParse parseIdentifier `shouldFailOn` "[  ]"
      testParse parseIdentifier `shouldFailOn` "[\t]"
    it "비어있는 식별자에 대해서 오류" $ do
      testParse parseIdentifier `shouldFailOn` "[]"

  describe "함수 이름 파싱" $ do
    it "띄어쓰기 없는 한 단어" $ do
      testParse parseFuncIdentifier "더하고" `shouldParse` "더하고"
    it "부울 키워드에 대해서 오류" $ do
      testParse parseFuncIdentifier `shouldFailOn` "참"
      testParse parseFuncIdentifier `shouldFailOn` "거짓"

  describe "함수 호출식 파싱" $ do
    it "인자가 2개인 함수 호출식" $ do
      testParse parseFuncCall "1과 2를 더하다"
        `shouldParse` funcCall
          (var "더하다")
          [(litInteger 1, "와"), (litInteger 2, "을")]
    it "조사가 없는 함수 호출식에 대해서 오류" $ do
      testParse parseFuncCall `shouldFailOn` "1 2를 더하다"
      testParse parseFuncCall `shouldFailOn` "(1 + 1) 피보나치 수 구하다"
    it "조사가 떨어져 있는 경우 오류" $ do
      testParse parseFuncCall `shouldFailOn` "1 과 2 를 더하다"
    it "인자가 없는 함수 호출식" $ do
      testParse parseFuncCall "던지다" `shouldParse` funcCall (var "던지다") []

  describe "소괄호 함수 호출식 파싱" $ do
    it "인자가 하나인 함수 호출식" $ do
      testParse (parseParenCall parseExpr) "더하다(1)"
        `shouldParse` funcCall
          (var "더하다")
          [(litInteger 1, "_")]
    it "인자가 두 개인 함수 호출식" $ do
      testParse (parseParenCall parseExpr) "더하다(1, 2)"
        `shouldParse` funcCall
          (var "더하다")
          [(litInteger 1, "_"), (litInteger 2, "_")]
    it "인자가 없는 함수 호출식" $ do
      testParse (parseParenCall parseExpr) "더하다()"
        `shouldParse` funcCall
          (var "더하다")
          []
    it "인자가 복합 표현식인 함수 호출식" $ do
      testParse (parseParenCall parseExpr) "더하다(1 + 2, 3와 4를 더하다)"
        `shouldParse` funcCall
          (var "더하다")
          [ (binaryOp Add (litInteger 1) (litInteger 2), "_"),
            (funcCall (var "더하다") [(litInteger 3, "와"), (litInteger 4, "을")], "_")
          ]
    it "함수 이름과 괄호 사이에 공백이 있으면 에러" $ do
      testParse (parseParenCall parseExpr) `shouldFailOn` "더하다 (1 + 2, 3와 4를 더하다)"

  describe "중첩된 함수 호출식 파싱" $ do
    it "한 번 중첩된 식" $ do
      testParse parseNestedFuncCalls "4와 2를 합하고 2로 나누다"
        `shouldParse` funcCall
          (var "나누다")
          [ ( funcCall
                (var "합하다")
                [(litInteger 4, "와"), (litInteger 2, "을")],
              "_"
            ),
            (litInteger 2, "로")
          ]
    it "두 번 중첩된 식" $ do
      testParse parseNestedFuncCalls "4와 2를 더하고 2로 나누고 3을 더하다"
        `shouldParse` funcCall
          (var "더하다")
          [ ( funcCall
                (var "나누다")
                [ ( funcCall
                      (var "더하다")
                      [(litInteger 4, "와"), (litInteger 2, "을")],
                    "_"
                  ),
                  (litInteger 2, "로")
                ],
              "_"
            ),
            (litInteger 3, "을")
          ]
    it "잘못된 동사 활용을 한 식" $ do
      testParse parseNestedFuncCalls `shouldFailOn` "4와 2를 더하다, 2로 나누다"

  describe "조건식 파싱" $ do
    it "단순 조건식 파싱" $ do
      testParse parseIf "만약 참 이라면 1 아니라면 2"
        `shouldParse` ifExpr (litBool True) (litInteger 1) (litInteger 2)
    it "연산식이 포함된 조건식 파싱" $ do
      testParse parseIf "만약 1 + 2 이라면 1 * 2 아니라면 2 / 3"
        `shouldParse` ifExpr
          (binaryOp Add (litInteger 1) (litInteger 2))
          (binaryOp Multiply (litInteger 1) (litInteger 2))
          (binaryOp Divide (litInteger 2) (litInteger 3))
    it "함수 호출식이 포함된 조건식 파싱" $ do
      testParse parseIf "만약 1과 2를 합하다 이라면 3을 던지고 받다 아니라면 2를 던지다"
        `shouldParse` ifExpr
          ( funcCall
              (var "합하다")
              [(litInteger 1, "와"), (litInteger 2, "을")]
          )
          ( funcCall
              (var "받다")
              [(funcCall (var "던지다") [(litInteger 3, "을")], "_")]
          )
          (funcCall (var "던지다") [(litInteger 2, "을")])
    it "논리 연산자를 사용한 조건식 파싱" $ do
      testParse parseIf "만약 1이 크다 그리고 2가 작다 또는 5가 크다 이라면 3 아니라면 4"
        `shouldParse` ifExpr
          ( binaryOp
              LogicOr
              ( binaryOp
                  LogicAnd
                  ( funcCall
                      (var "크다")
                      [(litInteger 1, "이")]
                  )
                  ( funcCall
                      (var "작다")
                      [(litInteger 2, "이")]
                  )
              )
              (funcCall (var "크다") [(litInteger 5, "이")])
          )
          (litInteger 3)
          (litInteger 4)

    it "중첩된 조건식 파싱" $ do
      testParse parseIf "만약 (만약 거짓 이라면 1 아니라면 2) 이라면 1 아니라면 2"
        `shouldParse` ifExpr
          (ifExpr (litBool False) (litInteger 1) (litInteger 2))
          (litInteger 1)
          (litInteger 2)

  describe "식 우선순위 테스트" $ do
    it "사칙연산 우선순위 괄호를 통해 변경" $ do
      testParse parseExpr "(1 + 1) / 2"
        `shouldParse` binaryOp
          Divide
          (binaryOp Add (litInteger 1) (litInteger 1))
          (litInteger 2)
    it "사칙연산식이 함수 호출식보다 우선순위 높음" $ do
      testParse parseExpr "1 + 1과 2를 더하다"
        `shouldParse` binaryOp
          Add
          (litInteger 1)
          (funcCall (var "더하다") [(litInteger 1, "와"), (litInteger 2, "을")])
    it "함수 호출식과 사칙연산식 우선순위 괄호를 통해 변경" $ do
      testParse parseExpr "(1 + 1)과 2를 더하다"
        `shouldParse` funcCall
          (var "더하다")
          [(binaryOp Add (litInteger 1) (litInteger 1), "와"), (litInteger 2, "을")]
    it "소괄호 함수 호출식이 함수 호출식보다 우선순위 높음" $ do
      testParse parseExpr "더하다(1, 2)와 3을 더하다"
        `shouldParse` funcCall
          (var "더하다")
          [ ( funcCall
                (var "더하다")
                [(litInteger 1, "_"), (litInteger 2, "_")],
              "와"
            ),
            (litInteger 3, "을")
          ]

  describe "시퀀스 파싱" $ do
    it "연산식 2개로 이어진 시퀀스" $ do
      testParse
        parseSeq
        ( [text| 
            순서대로
              1 + 1 
              2 * 2
          |]
        )
        `shouldParse` Seq
          [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
            Right $
              binaryOp Multiply (litInteger 2) (litInteger 2)
          ]

    it "비어있는 라인을 포함한 시퀀스" $ do
      testParse
        parseSeq
        ( [text|
            순서대로

              1
              1을 던지다
          |]
        )
        `shouldParse` Seq
          [ Right $ litInteger 1,
            Right $ funcCall (var "던지다") [(litInteger 1, "을")]
          ]
      testParse
        parseSeq
        ( [text|
            순서대로
              1
                                          
              1을 던지다
          |]
        )
        `shouldParse` Seq
          [ Right $ litInteger 1,
            Right $ funcCall (var "던지다") [(litInteger 1, "을")]
          ]
      testParse parseSeq "순서대로\n  1\n\n  1을 던지다"
        `shouldParse` Seq
          [ Right $ litInteger 1,
            Right $ funcCall (var "던지다") [(litInteger 1, "을")]
          ]

    it "중간에 함수 선언이 포함된 시퀀스" $ do
      testParse
        parseSeq
        ( [text|
            순서대로
              동사 [값]을 더하다:
                [값] + 1
              1
          |]
        )
        `shouldParse` Seq
          [ Left $
              funcDecl
                VerbDecl
                "더하다"
                [("값", "을")]
                (binaryOp Add (var "값") (litInteger 1)),
            Right $ litInteger 1
          ]
      testParse
        parseSeq
        ( [text|
            순서대로
              1 + 1
              동사 [값]을 더하다:
                [값] + 1

              함수 [값]을 빼다:
                [값] - 1

              1
          |]
        )
        `shouldParse` Seq
          [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
            Left $
              funcDecl
                VerbDecl
                "더하다"
                [("값", "을")]
                (binaryOp Add (var "값") (litInteger 1)),
            Left $
              funcDecl
                NormalDecl
                "빼다"
                [("값", "을")]
                (binaryOp Subtract (var "값") (litInteger 1)),
            Right $ litInteger 1
          ]

    it "마지막이 선언문인 시퀀스에 대해서 오류" $ do
      testParse
        parseSeq
        `shouldFailOn` ( [text|
            순서대로
              형용사 [값1]과 [값2]가 같다:
                [값1] + 1
              1
              상수 [수]: 10 + 10
          |]
                       )
