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
                              1
                              (ann [Inst.Load 0, Inst.Push 0, Inst.Add])
                              (S.fromList [ConstInteger 1])
                            )
                        ]
                      , [Inst.Push 0, Inst.StoreGlobal 0]
                      )
      it "인자가 두 개인 함수 선언 코드 생성"
        $             compileStmt
                        (funcDecl "더하다" ["수1", "수2"] (binaryOp Add (var "수1") (var "수2")))
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              2
                              (ann [Inst.Load 0, Inst.Load 1, Inst.Add])
                              S.empty
                            )
                        ]
                      , [Inst.Push 0, Inst.StoreGlobal 0]
                      )
      it "외부에 전역 변수가 있는 함수 선언 코드 생성"
        $             do
                        compileStmt (constDecl "값" (litInteger 1))
                        compileStmt
                          (funcDecl "더하다" ["수"] (binaryOp Add (var "수") (var "값")))
        `shouldBuild` ( S.fromList
                        [ ConstInteger 1
                        , ConstFunc
                          (FuncObject
                            1
                            (ann [Inst.Load 0, Inst.LoadGlobal 0, Inst.Add])
                            S.empty
                          )
                        ]
                      , [ Inst.Push 0
                        , Inst.StoreGlobal 0
                        , Inst.Push 1
                        , Inst.StoreGlobal 1
                        ]
                      )
      it "외부에 전역 변수가 있을 때 변수 섀도잉 하는 함수 선언 코드 생성"
        $             do
                        compileStmt (constDecl "값" (litInteger 1))
                        compileStmt
                          (funcDecl "더하다" ["값"] (binaryOp Add (var "값") (litInteger 2)))
        `shouldBuild` ( S.fromList
                        [ ConstInteger 1
                        , ConstFunc
                          (FuncObject
                            1
                            (ann [Inst.Load 0, Inst.Push 0, Inst.Add])
                            (S.singleton (ConstInteger 2))
                          )
                        ]
                      , [ Inst.Push 0
                        , Inst.StoreGlobal 0
                        , Inst.Push 1
                        , Inst.StoreGlobal 1
                        ]
                      )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1]
                      , [Inst.Push 0, Inst.StoreGlobal 0]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2]
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.StoreGlobal 0]
                      )

