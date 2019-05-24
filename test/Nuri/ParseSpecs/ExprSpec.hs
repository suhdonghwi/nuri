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

spec :: Spec
spec = do
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
    it "두 정수 더하기" $ do
      testParse arithmetic "1 + 2"
        `shouldParse` binaryOp Add (litInteger 1) (litInteger 2)
    it "(붙어있는) 두 정수 더하기" $ do
      testParse arithmetic "1+2"
        `shouldParse` binaryOp Add (litInteger 1) (litInteger 2)
    it "두 정수 빼기" $ do
      testParse arithmetic "2 - 4"
        `shouldParse` binaryOp Subtract (litInteger 2) (litInteger 4)
    it "(붙어있는) 두 정수 빼기" $ do
      testParse arithmetic "2-4"
        `shouldParse` binaryOp Subtract (litInteger 2) (litInteger 4)
    it "두 정수 곱하기" $ do
      testParse arithmetic "2 * 4"
        `shouldParse` binaryOp Multiply (litInteger 2) (litInteger 4)
    it "두 정수 나누기" $ do
      testParse arithmetic "8 / 2"
        `shouldParse` binaryOp Divide (litInteger 8) (litInteger 2)

