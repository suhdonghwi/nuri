module Nuri.Spec.Codegen.StmtSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Codegen.Stmt

import qualified Haneul.Instruction            as Inst
import           Haneul.Builder
import           Haneul.Constant

spec :: Spec
spec = do
  describe "표현식 구문 코드 생성" $ do
    it "일반 연산 표현식 구문" $ do
      compileStmt (ExprStmt (litInteger 10))
        `shouldBuild` ( defaultI { _constTable = S.singleton (ConstInteger 10) }
                      , [Inst.Push 0, Inst.Pop]
                      )
    it "1개 이상의 일반 연산 표현식 구문"
      $             (do
                      compileStmt (ExprStmt (litInteger 10))
                      compileStmt (ExprStmt (binaryOp Add (litInteger 20) (litInteger 30)))
                    )
      `shouldBuild` ( defaultI
                      { _constTable =
                        S.fromList
                          [ConstInteger 10, ConstInteger 20, ConstInteger 30]
                      }
                    , [ Inst.Push 0
                      , Inst.Pop
                      , Inst.Push 1
                      , Inst.Push 2
                      , Inst.Add
                      , Inst.Pop
                      ]
                    )
  describe "대입 구문 코드 생성" $ do
    it "정수 대입 코드 생성" $ do
      compileStmt (assign "값" (litInteger 10))
        `shouldBuild` ( defaultI { _constTable = S.singleton (ConstInteger 10)
                                 , _varNames   = S.singleton "값"
                                 }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "연산삭 대입 코드 생성" $ do
      compileStmt (assign "값" (binaryOp Add (litInteger 10) (litInteger 20)))
        `shouldBuild` ( defaultI
                        { _constTable = S.fromList
                                          [ConstInteger 10, ConstInteger 20]
                        , _varNames   = S.singleton "값"
                        }
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.Store 0]
                      )
