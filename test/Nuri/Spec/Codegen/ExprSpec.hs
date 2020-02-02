{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Codegen.ExprSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Expr
import           Nuri.Codegen.Expr

import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( Marked(Value) )
import           Haneul.Constant

spec :: Spec
spec = do
  describe "리터럴 코드 생성" $ do
    it "정수 리터럴 코드 생성" $ do
      compileExpr (litInteger 10)
        `shouldBuild` (S.fromList [ConstNone, ConstInteger 10], [Inst.Push 1])
    it "리터럴 여러개 코드 생성"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                    )
      `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstChar 'a']
                    , [Inst.Push 1, Inst.Push 2]
                    )
    it "리터럴 여러개 코드 생성 (중복 포함)"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                      compileExpr (litInteger 10)
                    )
      `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstChar 'a']
                    , [Inst.Push 1, Inst.Push 2, Inst.Push 1]
                    )
  describe "조건식 코드 생성" $ do
    it "단순 리터럴에 대한 조건식 코드 생성"
      $ compileExpr (ifExpr (litBool True) (litInteger 1) (litInteger 2))
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstBool True
                      , ConstInteger 1
                      , ConstInteger 2
                      ]
                    , [ Inst.Push 1
                      , Inst.PopJmpIfFalse (Value 4)
                      , Inst.Push 2
                      , Inst.Jmp (Value 5)
                      , Inst.Push 3
                      ]
                    )
    it "계산식이 포함된 조건식 코드 생성"
      $             compileExpr
                      (ifExpr (binaryOp Equal (litInteger 1) (litInteger 2))
                              (binaryOp Add (litInteger 3) (litInteger 4))
                              (litInteger 5)
                      )
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstInteger 1
                      , ConstInteger 2
                      , ConstInteger 3
                      , ConstInteger 4
                      , ConstInteger 5
                      ]
                    , [ Inst.Push 1
                      , Inst.Push 2
                      , Inst.Equal
                      , Inst.PopJmpIfFalse (Value 8)
                      , Inst.Push 3
                      , Inst.Push 4
                      , Inst.Add
                      , Inst.Jmp (Value 9)
                      , Inst.Push 5
                      ]
                    )
  describe "이항 연산 코드 생성" $ do
    it "덧셈 코드 생성" $ do
      compileExpr (binaryOp Add (litInteger 10) (litInteger 20))
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 20]
                      , [Inst.Push 1, Inst.Push 2, Inst.Add]
                      )
    it "복합 연산 코드 생성" $ do
      compileExpr
          (binaryOp Add
                    (litInteger 10)
                    (binaryOp Divide (litInteger 20) (litInteger 30))
          )
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstInteger 10
                        , ConstInteger 20
                        , ConstInteger 30
                        ]
                      , [ Inst.Push 1
                        , Inst.Push 2
                        , Inst.Push 3
                        , Inst.Divide
                        , Inst.Add
                        ]
                      )
  describe "단항 연산 코드 생성" $ do
    it "양수 코드 생성" $ do
      compileExpr (unaryOp Positive (litInteger 10))
        `shouldBuild` (S.fromList [ConstNone, ConstInteger 10], [Inst.Push 1])
    it "음수 코드 생성" $ do
      compileExpr (unaryOp Negative (litInteger 10))
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.Push 1, Inst.Negate]
                      )

  describe "변수 접근 코드 생성" $ do
    it "변수 이름 접근하는 Load 코드 생성" $ do
      compileExpr (var "값")
        `shouldBuild` (S.singleton ConstNone, [Inst.Load "값"])

  describe "함수 호출 코드 생성" $ do
    it "인수가 하나인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.Load "던지다", Inst.Push 1, Inst.Call 1]
                      )
    it "인수가 3개인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10, litInteger 10, litInteger 0])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 0]
                      , [ Inst.Load "던지다"
                        , Inst.Push 1
                        , Inst.Push 1
                        , Inst.Push 2
                        , Inst.Call 3
                        ]
                      )
    it "인수가 없는 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [])
        `shouldBuild` (S.singleton ConstNone, [Inst.Load "던지다", Inst.Call 0])
  describe "표현식 시퀀스 코드 생성" $ do
    it "표현식이 1개인 시퀀스 코드 생성" $ do
      compileExpr (Seq [litInteger 10])
        `shouldBuild` (S.fromList [ConstNone, ConstInteger 10], [Inst.Push 1])
    it "표현식이 2개인 시퀀스 코드 생성" $ do
      compileExpr (Seq [litInteger 10, litBool True])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstBool True]
                      , [Inst.Push 1, Inst.Pop, Inst.Push 2]
                      )

