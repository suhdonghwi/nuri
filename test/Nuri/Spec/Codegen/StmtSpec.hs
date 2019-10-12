module Nuri.Spec.Codegen.StmtSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Literal
import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Codegen.Stmt

import qualified Haneul.Instruction            as Inst

spec :: Spec
spec = do
  describe "표현식 구문 코드 생성" $ do
    it "일반 연산 표현식 구문" $ do
      compileStmt (ExprStmt (litInteger 10))
        `shouldBuild` (S.singleton (LitInteger 10), [Inst.Push 0, Inst.Pop])
    it "1개 이상의 일반 연산 표현식 구문"
      $             (do
                      compileStmt (ExprStmt (litInteger 10))
                      compileStmt (ExprStmt (binaryOp Add (litInteger 20) (litInteger 30)))
                    )
      `shouldBuild` ( S.fromList [LitInteger 10, LitInteger 20, LitInteger 30]
                    , [ Inst.Push 0
                      , Inst.Pop
                      , Inst.Push 1
                      , Inst.Push 2
                      , Inst.Add
                      , Inst.Pop
                      ]
                    )
