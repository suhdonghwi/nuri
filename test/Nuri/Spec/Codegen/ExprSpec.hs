module Nuri.Spec.Codegen.ExprSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Literal
import           Nuri.Expr
import           Nuri.Codegen.Expr

import           Haneul.Instruction

spec :: Spec
spec = do
  describe "리터럴 코드 생성" $ do
    it "정수 리터럴 코드 생성" $ do
      compileExpr (litInteger 10)
        `shouldBuild` (S.singleton (LitInteger 10), [Push 0])
    it "리터럴 여러개 코드 생성"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                    )
      `shouldBuild` (S.fromList [LitInteger 10, LitChar 'a'], [Push 0, Push 1])
    it "리터럴 여러개 코드 생성 (중복 포함)"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                      compileExpr (litInteger 10)
                    )
      `shouldBuild` ( S.fromList [LitInteger 10, LitChar 'a']
                    , [Push 0, Push 1, Push 0]
                    )
  describe "이항 연산 코드 생성" $ do
    it "덧셈 코드 생성" $ do
      compileExpr (binaryOp Plus (litInteger 10) (litInteger 20))
        `shouldBuild` ( S.fromList [LitInteger 10, LitInteger 20]
                      , [Push 0, Push 1, Add]
                      )
    it "복합 연산 코드 생성" $ do
      compileExpr
          (binaryOp Plus
                    (litInteger 10)
                    (binaryOp Slash (litInteger 20) (litInteger 30))
          )
        `shouldBuild` ( S.fromList [LitInteger 10, LitInteger 20, LitInteger 30]
                      , [Push 0, Push 1, Push 2, Divide, Add]
                      )

