module Nuri.ParseSpecs.ExprSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "이진 " $ do
    it "should parse " $ do
      1 `shouldBe` 1