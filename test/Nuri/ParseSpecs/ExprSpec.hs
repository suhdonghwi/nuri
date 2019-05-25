module Nuri.ParseSpecs.ExprSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Text.Megaparsec

import           Nuri.Parse.Expr
import           Nuri.ParseSpecs.Util
import           Nuri.Expr

p = initialPos "(test)"
litInteger i = Lit p (LitInteger i)
binaryOp = BinaryOp p
unaryOp = UnaryOp p
var = Var p

spec :: Spec
spec = do
  describe "정수 파싱" $ do
    describe "2진수 파싱" $ do
      it "0b1010을 10으로 파싱" $ do
        testParse binary "0b1010" `shouldParse` 10
      it "0B1010을 10으로 파싱" $ do
        testParse binary "0B1010" `shouldParse` 10
      it "0b0000을 0으로 파싱" $ do
        testParse binary "0b0000" `shouldParse` 0

    describe "8진수 파싱" $ do
      it "012을 10으로 파싱" $ do
        testParse octal "012" `shouldParse` 10
      it "000을 0으로 파싱" $ do
        testParse octal "000" `shouldParse` 0

    describe "10진수 파싱" $ do
      it "10을 10으로 파싱" $ do
        testParse decimal "10" `shouldParse` 10
      it "0을 0으로 파싱" $ do
        testParse decimal "0" `shouldParse` 0

    describe "16진수 파싱" $ do
      it "0x000A를 10으로 파싱" $ do
        testParse hexadecimal "0x000A" `shouldParse` 10
      it "0x0010을 16으로 파싱" $ do
        testParse hexadecimal "0x0010" `shouldParse` 16
      it "0xffFF를 65535으로 파싱" $ do
        testParse hexadecimal "0xffFF" `shouldParse` 65535
      it "0XffFF를 65535으로 파싱" $ do
        testParse hexadecimal "0XffFF" `shouldParse` 65535
      it "0x0000을 0으로 파싱" $ do
        testParse hexadecimal "0x0000" `shouldParse` 0

    describe "정수 파싱" $ do
      it "0b1010을 10으로 파싱" $ do
        testParse integer "0b1010" `shouldParse` litInteger 10
      it "012을 10으로 파싱" $ do
        testParse integer "012" `shouldParse` litInteger 10
      it "10을 10으로 파싱" $ do
        testParse integer "10" `shouldParse` litInteger 10
      it "0x000A를 10으로 파싱" $ do
        testParse integer "0x000A" `shouldParse` litInteger 10
      it "0을 0으로 파싱" $ do
        testParse integer "0" `shouldParse` litInteger 0
      it "00을 0으로 파싱" $ do
        testParse integer "00" `shouldParse` litInteger 0

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
      testParse identifier "[foo]" `shouldParse` var "foo"
    it "(대문자) 영문 식별자" $ do
      testParse identifier "[Foo]" `shouldParse` var "Foo"
    it "한글 식별자" $ do
      testParse identifier "[사과]" `shouldParse` var "사과"
    it "숫자가 포함된 식별자" $ do
      testParse identifier "[사람2]" `shouldParse` var "사람2"
    it "공백이 포함된 식별자" $ do
      testParse identifier "[사과는 맛있다]" `shouldParse` var "사과는 맛있다"
    it "숫자로 시작하는 식별자에 대해 오류" $ do
      testParse identifier `shouldFailOn` "[10마리 펭귄]"
    it "공백으로 시작하는 식별자에 대해 오류" $ do
      testParse identifier `shouldFailOn` "[ Foo]"
    it "공백만 포함된 식별자에 대해서 오류" $ do
      testParse identifier `shouldFailOn` "[  ]"
    it "비어있는 식별자에 대해서 오류" $ do
      testParse identifier `shouldFailOn` "[]"

