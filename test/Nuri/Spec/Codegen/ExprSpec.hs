module Nuri.Spec.Codegen.ExprSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Expr
import           Nuri.Codegen.Expr

import qualified Haneul.Instruction            as Inst
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
                      compileExpr (litString "a")
                    )
      `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstString "a"]
                    , [Inst.Push 1, Inst.Push 2]
                    )
    it "리터럴 여러개 코드 생성 (중복 포함)"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litString "a")
                      compileExpr (litInteger 10)
                    )
      `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstString "a"]
                    , [Inst.Push 1, Inst.Push 2, Inst.Push 1]
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
    it "선언되지 않은 변수 이름에 대해 LoadGlobal 코드 생성" $ do
      compileExpr (var "값")
        `shouldBuild` (S.singleton ConstNone, [Inst.LoadGlobal "값"])

  describe "함수 호출 코드 생성" $ do
    it "인수가 하나인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.LoadGlobal "던지다", Inst.Push 1, Inst.Call 1]
                      )
    it "인수가 3개인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10, litInteger 10, litInteger 0])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 0]
                      , [ Inst.LoadGlobal "던지다"
                        , Inst.Push 1
                        , Inst.Push 1
                        , Inst.Push 2
                        , Inst.Call 3
                        ]
                      )
    it "인수가 없는 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [])
        `shouldBuild` ( S.singleton ConstNone
                      , [Inst.LoadGlobal "던지다", Inst.Call 0]
                      )
  describe "목록 표현식 코드 생성" $ do
    it "원소가 정수 하나인 목록 코드 생성" $ do
      compileExpr (list [litInteger 10])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.Push 1, Inst.BuildList 1]
                      )
    it "원소가 정수 두 개인 목록 코드 생성" $ do
      compileExpr (list [litInteger 10, litInteger 20])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 20]
                      , [Inst.Push 1, Inst.Push 2, Inst.BuildList 2]
                      )
    it "표현식이 포함된 목록 코드 생성" $ do
      compileExpr
          (list [binaryOp Add (litInteger 10) (litInteger 20), litInteger 20])
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 20]
                      , [ Inst.Push 1
                        , Inst.Push 2
                        , Inst.Add
                        , Inst.Push 2
                        , Inst.BuildList 2
                        ]
                      )
    it "비어있는 목록 코드 생성" $ do
      compileExpr (list [])
        `shouldBuild` (S.singleton ConstNone, [Inst.BuildList 0])

