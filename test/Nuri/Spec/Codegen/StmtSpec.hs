{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Codegen.StmtSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Expr
import           Nuri.Codegen.Stmt

import qualified Haneul.Instruction            as Inst
import           Haneul.Constant

spec :: Spec
spec = do
  describe "선언문 코드 생성" $ do
    describe "함수 선언 코드 생성" $ do
      it "인자가 하나인 함수 선언 코드 생성"
        $             compileStmt
                        (funcDecl "더하다" ["값"] (binaryOp Add (var "값") (litInteger 1)))
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["값"]
                              (ann [Inst.Load "값", Inst.Push 0, Inst.Add])
                              (S.fromList [ConstInteger 1])
                            )
                        ]
                      , [Inst.Push 0, Inst.Store "더하다"]
                      )
      it "인자가 두 개인 함수 선언 코드 생성"
        $             compileStmt
                        (funcDecl "더하다" ["수1", "수2"] (binaryOp Add (var "수1") (var "수2")))
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["수1", "수2"]
                              (ann [Inst.Load "수1", Inst.Load "수2", Inst.Add])
                              S.empty
                            )
                        ]
                      , [Inst.Push 0, Inst.Store "더하다"]
                      )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1]
                      , [Inst.Push 0, Inst.Store "값"]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2]
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.Store "값"]
                      )

