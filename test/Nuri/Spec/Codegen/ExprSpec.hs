module Nuri.Spec.Codegen.ExprSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Literal
import           Nuri.Expr
import           Nuri.Codegen.Expr

import qualified Haneul.Instruction            as Inst

spec :: Spec
spec = do
  describe "리터럴 코드 생성" $ do
    it "정수 리터럴 코드 생성" $ do
      compileExpr (litInteger 10)
        `shouldBuild` (S.singleton (LitInteger 10), [Inst.Push 0])
    it "리터럴 여러개 코드 생성"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                    )
      `shouldBuild` ( S.fromList [LitInteger 10, LitChar 'a']
                    , [Inst.Push 0, Inst.Push 1]
                    )
    it "리터럴 여러개 코드 생성 (중복 포함)"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                      compileExpr (litInteger 10)
                    )
      `shouldBuild` ( S.fromList [LitInteger 10, LitChar 'a']
                    , [Inst.Push 0, Inst.Push 1, Inst.Push 0]
                    )
  describe "이항 연산 코드 생성" $ do
    it "덧셈 코드 생성" $ do
      compileExpr (binaryOp Add (litInteger 10) (litInteger 20))
        `shouldBuild` ( S.fromList [LitInteger 10, LitInteger 20]
                      , [Inst.Push 0, Inst.Push 1, Inst.Add]
                      )
    it "복합 연산 코드 생성" $ do
      compileExpr
          (binaryOp Add
                    (litInteger 10)
                    (binaryOp Divide (litInteger 20) (litInteger 30))
          )
        `shouldBuild` ( S.fromList [LitInteger 10, LitInteger 20, LitInteger 30]
                      , [ Inst.Push 0
                        , Inst.Push 1
                        , Inst.Push 2
                        , Inst.Divide
                        , Inst.Add
                        ]
                      )
  describe "단항 연산 코드 생성" $ do
    it "양수 코드 생성" $ do
      compileExpr (unaryOp Positive (litInteger 10))
        `shouldBuild` (S.fromList [LitInteger 10], [Inst.Push 0])
    it "음수 코드 생성" $ do
      compileExpr (unaryOp Negative (litInteger 10))
        `shouldBuild` (S.fromList [LitInteger 10], [Inst.Push 0, Inst.Negate])

