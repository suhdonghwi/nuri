module Nuri.ParseSpecs.ExprSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec

import Nuri.Parse.Expr

spec :: Spec
spec = do
  describe "2진수 파싱 " $ do
    it "0b1010을 10으로 파싱" $ do
      parse binary "" "0b1010" `shouldParse` 10
    it "0b0000을 0으로 파싱" $ do
      parse binary "" "0b0000" `shouldParse` 0
  describe "8진수 파싱 " $ do
    it "012을 10으로 파싱" $ do
      parse octal "" "012" `shouldParse` 10
    it "000을 0으로 파싱" $ do
      parse octal "" "000" `shouldParse` 0
  describe "10진수 파싱" $ do
    it "10을 10으로 파싱" $ do
      parse decimal "" "10" `shouldParse` 10
    it "0을 0으로 파싱" $ do
      parse decimal "" "0" `shouldParse` 0
  describe "16진수 파싱" $ do
    it "0x000A를 10으로 파싱" $ do
      parse hexadecimal "" "0x000A" `shouldParse` 10
    it "0x0010을 16으로 파싱" $ do
      parse hexadecimal "" "0x0010" `shouldParse` 16
    it "0xffFF를 65535으로 파싱" $ do
      parse hexadecimal "" "0xffFF" `shouldParse` 65535
    it "0x0000을 0으로 파싱" $ do
      parse hexadecimal "" "0x0000" `shouldParse` 0