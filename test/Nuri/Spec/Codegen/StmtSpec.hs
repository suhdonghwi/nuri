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
                      , [Inst.Push 0, storeGlobal 0]
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
                      , [Inst.Push 0, storeGlobal 0]
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
                            (ann [Inst.Load 0, loadGlobal 0, Inst.Add])
                            S.empty
                          )
                        ]
                      , [Inst.Push 0, storeGlobal 0, Inst.Push 1, storeGlobal 1]
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
                      , [Inst.Push 0, storeGlobal 0, Inst.Push 1, storeGlobal 1]
                      )
      it "외부 로컬 스코프에 있는 변수 캡쳐하는 클로저 함수 코드 생성"
        $             do
                        compileStmt
                          (funcDecl
                            "더하다"
                            ["값1"]
                            (letExpr "값2"
                                     (litInteger 1)
                                     (binaryOp Add (var "값1") (var "값2"))
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              1
                              (ann
                                [ Inst.Push 0
                                , Inst.Push 1
                                , Inst.FreeVarLocal 0
                                , Inst.Call 1
                                ]
                              )
                              (S.fromList
                                [ ConstInteger 1
                                , ConstFunc
                                  (FuncObject
                                    1
                                    (ann
                                      [Inst.LoadDeref 0, Inst.Load 0, Inst.Add]
                                    )
                                    S.empty
                                  )
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, storeGlobal 0]
                      )
      it "3개 이상의 스코프가 중첩된 클로저 코드 생성"
        $             do
                        compileStmt
                          (funcDecl
                            "바깥"
                            ["값"]
                            (letExpr
                              "중간"
                              (lambda [] (letExpr "안쪽" (lambda [] (var "값")) (var "안쪽")))
                              (var "중간")
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              1
                              (ann
                                [ Inst.Push 0
                                , Inst.FreeVarLocal 0
                                , Inst.Push 1
                                , Inst.Call 1
                                ]
                              )
                              (S.fromList
                                [ ConstFunc
                                  (FuncObject
                                    0
                                    (ann
                                      [ Inst.Push 0
                                      , Inst.FreeVarFree 0
                                      , Inst.Push 1
                                      , Inst.Call 1
                                      ]
                                    )
                                    (S.fromList
                                      [ ConstFunc
                                        (FuncObject 0
                                                    (ann [Inst.LoadDeref 0])
                                                    S.empty
                                        )
                                      , ConstFunc
                                        (FuncObject 1
                                                    (ann [Inst.Load 0])
                                                    S.empty
                                        )
                                      ]
                                    )
                                  )
                                , ConstFunc
                                  (FuncObject 1 (ann [Inst.Load 0]) S.empty)
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, Inst.StoreGlobal 1]
                      )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1]
                      , [Inst.Push 0, storeGlobal 0]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileStmt (constDecl "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2]
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, storeGlobal 0]
                      )

