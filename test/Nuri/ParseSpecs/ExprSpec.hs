module Nuri.ParseSpecs.ExprSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Parse.Expr
import           Nuri.Util
import           Nuri.Expr

spec :: Spec
spec = do
  describe "정수 파싱" $ do
    describe "2진수 파싱" $ do
      it "0b1010을 10으로 파싱" $ do
        testParse integer "0b1010" `shouldParse` litInteger 10
      it "0B1010을 10으로 파싱" $ do
        testParse integer "0B1010" `shouldParse` litInteger 10
      it "0b0000을 0으로 파싱" $ do
        testParse integer "0b0000" `shouldParse` litInteger 0

    describe "8진수 파싱" $ do
      it "012을 10으로 파싱" $ do
        testParse integer "012" `shouldParse` litInteger 10
      it "000을 0으로 파싱" $ do
        testParse integer "000" `shouldParse` litInteger 0

    describe "10진수 파싱" $ do
      it "10을 10으로 파싱" $ do
        testParse integer "10" `shouldParse` litInteger 10
      it "0을 0으로 파싱" $ do
        testParse integer "0" `shouldParse` litInteger 0

    describe "16진수 파싱" $ do
      it "0x000A를 10으로 파싱" $ do
        testParse integer "0x000A" `shouldParse` litInteger 10
      it "0x0010을 16으로 파싱" $ do
        testParse integer "0x0010" `shouldParse` litInteger 16
      it "0xffFF를 65535으로 파싱" $ do
        testParse integer "0xffFF" `shouldParse` litInteger 65535
      it "0XffFF를 65535으로 파싱" $ do
        testParse integer "0XffFF" `shouldParse` litInteger 65535
      it "0x0000을 0으로 파싱" $ do
        testParse integer "0x0000" `shouldParse` litInteger 0

  describe "사칙연산식 파싱" $ do
    describe "단항 연산자" $ do
      it "양의 부호 정수" $ do
        testParse arithmetic "+2" `shouldParse` unaryOp Plus (litInteger 2)
      it "(떨어져 있는) 양의 부호 정수" $ do
        testParse arithmetic "+ 2" `shouldParse` unaryOp Plus (litInteger 2)
      it "음의 부호 정수" $ do
        testParse arithmetic "-2" `shouldParse` unaryOp Minus (litInteger 2)
      it "(떨어져 있는) 음의 부호 정수" $ do
        testParse arithmetic "- 2" `shouldParse` unaryOp Minus (litInteger 2)
    describe "이항 연산자" $ do
      it "두 정수 더하기" $ do
        testParse arithmetic "1 + 2"
          `shouldParse` binaryOp Plus (litInteger 1) (litInteger 2)
      it "(붙어있는) 두 정수 더하기" $ do
        testParse arithmetic "1+2"
          `shouldParse` binaryOp Plus (litInteger 1) (litInteger 2)
      it "(부호있는) 두 정수 더하기" $ do
        testParse arithmetic "1++2"
          `shouldParse` binaryOp
                          Plus
                          (litInteger 1)
                          (unaryOp Plus (litInteger 2))
      it "두 정수 빼기" $ do
        testParse arithmetic "2 - 4"
          `shouldParse` binaryOp Minus (litInteger 2) (litInteger 4)
      it "(붙어있는) 두 정수 빼기" $ do
        testParse arithmetic "2-4"
          `shouldParse` binaryOp Minus (litInteger 2) (litInteger 4)
      it "(부호있는) 두 정수 빼기" $ do
        testParse arithmetic "1--2"
          `shouldParse` binaryOp
                          Minus
                          (litInteger 1)
                          (unaryOp Minus (litInteger 2))
      it "두 정수 곱하기" $ do
        testParse arithmetic "2 * 4"
          `shouldParse` binaryOp Asterisk (litInteger 2) (litInteger 4)
      it "두 정수 나누기" $ do
        testParse arithmetic "8 / 2"
          `shouldParse` binaryOp Slash (litInteger 8) (litInteger 2)
    describe "복합 연산" $ do
      it "두 정수 더하고 한 정수로 나누기" $ do
        testParse arithmetic "4 / 2 + 3" `shouldParse` binaryOp
          Plus
          (binaryOp Slash (litInteger 4) (litInteger 2))
          (litInteger 3)
      it "두 정수 더하고 한 정수로 나누기 (순서 바꿔서)" $ do
        testParse arithmetic "3 + 4 / 2" `shouldParse` binaryOp
          Plus
          (litInteger 3)
          (binaryOp Slash (litInteger 4) (litInteger 2))

  describe "식별자 파싱" $ do
    it "영문 식별자" $ do
      testParse identifierExpr "[foo]" `shouldParse` var "foo"
    it "(대문자) 영문 식별자" $ do
      testParse identifierExpr "[Foo]" `shouldParse` var "Foo"
    it "한글 음절 식별자" $ do
      testParse identifierExpr "[사과]" `shouldParse` var "사과"
    it "한글 자음 식별자" $ do
      testParse identifierExpr "[ㅅㄷㅎ]" `shouldParse` var "ㅅㄷㅎ"
    it "한글 모음 식별자" $ do
      testParse identifierExpr "[ㅓㅗㅜㅣ]" `shouldParse` var "ㅓㅗㅜㅣ"
    it "숫자가 포함된 식별자" $ do
      testParse identifierExpr "[사람2]" `shouldParse` var "사람2"
    it "공백이 포함된 식별자" $ do
      testParse identifierExpr "[사과는 맛있다]" `shouldParse` var "사과는 맛있다"
    it "숫자로 시작하는 식별자에 대해 오류" $ do
      testParse identifierExpr `shouldFailOn` "[10마리 펭귄]"
    it "공백으로 시작하는 식별자에 대해 오류" $ do
      testParse identifierExpr `shouldFailOn` "[ Foo]"
    it "공백만 포함된 식별자에 대해서 오류" $ do
      testParse identifierExpr `shouldFailOn` "[  ]"
    it "비어있는 식별자에 대해서 오류" $ do
      testParse identifierExpr `shouldFailOn` "[]"

  describe "함수 이름 파싱" $ do
    it "더하고" $ do
      testParse funcIdentifier "더하고" `shouldParse` "더하고"
    it "반환 키워드에 대해서 오류" $ do
      testParse funcIdentifier `shouldFailOn` "반환하다"

  describe "함수 호출식 파싱" $ do
    it "인자가 2개인 함수 호출식" $ do
      testParse funcCall "1 2 더하다"
        `shouldParse` app "더하다" [litInteger 1, litInteger 2]
    it "인자가 없는 함수 호출식" $ do
      testParse funcCall "깨우다" `shouldParse` app "깨우다" []

  describe "중첩된 함수 호출식 파싱" $ do
    it "한 번 중첩된 식" $ do
      testParse nestedFuncCalls "4 2 더하고 2 나누다"
        `shouldParse` app
                        "나누다"
                        [app "더하고" [litInteger 4, litInteger 2], litInteger 2]
    it "두 번 중첩된 식" $ do
      testParse nestedFuncCalls "4 2 더하고 2 나누고 3 더하다" `shouldParse` app
        "더하다"
        [ app "나누고" [app "더하고" [litInteger 4, litInteger 2], litInteger 2]
        , litInteger 3
        ]

  describe "식 우선순위 테스트" $ do
    it "사칙연산 우선순위 괄호를 통해 변경" $ do
      testParse expr "(1 + 1) / 2" `shouldParse` binaryOp
        Slash
        (binaryOp Plus (litInteger 1) (litInteger 1))
        (litInteger 2)
    it "함수 호출식이 사칙연산식보다 우선순위 높음" $ do
      testParse expr "1 + 1 2 더하다" `shouldParse` binaryOp
        Plus
        (litInteger 1)
        (app "더하다" [litInteger 1, litInteger 2])
    it "함수 호출식과 사칙연산식 우선순위 괄호를 통해 변경" $ do
      testParse expr "(1 + 1) 2 더하다" `shouldParse` app
        "더하다"
        [binaryOp Plus (litInteger 1) (litInteger 1), litInteger 2]
