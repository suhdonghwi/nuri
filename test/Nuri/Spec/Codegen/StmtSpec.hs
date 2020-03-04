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
                        (funcDeclStmt "더하다"
                                      [("값", "을")]
                                      (binaryOp Add (var "값") (litInteger 1))
                        )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["을"]
                              2
                              0
                              (S.fromList [ConstInteger 1])
                              (ann [Inst.Load 0, Inst.Push 0, Inst.Add])
                            )
                        ]
                      , [Inst.Push 0, storeGlobal 0]
                      )
      it "인자가 두 개인 함수 선언 코드 생성"
        $             compileStmt
                        (funcDeclStmt "더하다"
                                      [("수1", "에"), ("수2", "을")]
                                      (binaryOp Add (var "수1") (var "수2"))
                        )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["에", "을"]
                              2
                              0
                              S.empty
                              (ann [Inst.Load 0, Inst.Load 1, Inst.Add])
                            )
                        ]
                      , [Inst.Push 0, storeGlobal 0]
                      )
      it "외부에 전역 변수가 있는 함수 선언 코드 생성"
        $             do
                        compileStmt (constDeclStmt "값" (litInteger 1))
                        compileStmt
                          (funcDeclStmt "더하다"
                                        [("수", "을")]
                                        (binaryOp Add (var "수") (var "값"))
                          )
        `shouldBuild` ( S.fromList
                        [ ConstInteger 1
                        , ConstFunc
                          (FuncObject
                            ["을"]
                            2
                            0
                            S.empty
                            (ann [Inst.Load 0, loadGlobal 0, Inst.Add])
                          )
                        ]
                      , [Inst.Push 0, storeGlobal 0, Inst.Push 1, storeGlobal 1]
                      )
      it "외부에 전역 변수가 있을 때 변수 섀도잉 하는 함수 선언 코드 생성"
        $             do
                        compileStmt (constDeclStmt "값" (litInteger 1))
                        compileStmt
                          (funcDeclStmt "더하다"
                                        [("값", "을")]
                                        (binaryOp Add (var "값") (litInteger 2))
                          )
        `shouldBuild` ( S.fromList
                        [ ConstInteger 1
                        , ConstFunc
                          (FuncObject
                            ["을"]
                            2
                            0
                            (S.singleton (ConstInteger 2))
                            (ann [Inst.Load 0, Inst.Push 0, Inst.Add])
                          )
                        ]
                      , [Inst.Push 0, storeGlobal 0, Inst.Push 1, storeGlobal 1]
                      )
      it "함수의 인자를 참조하는 함수 코드 생성"
        $             do
                        compileStmt
                          (funcDeclStmt
                            "더하다"
                            [("값1", "을")]
                            (Seq
                              [ Left $ constDecl "값2" (litInteger 1)
                              , Right $ binaryOp Add (var "값1") (var "값2")
                              ]
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["을"]
                              2
                              2
                              (S.fromList [ConstInteger 1])
                              (ann
                                [ Inst.Push 0
                                , Inst.Store 1
                                , Inst.Load 0
                                , Inst.Load 1
                                , Inst.Add
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, storeGlobal 0]
                      )
      it "시퀀스의 마지막이 선언 문인 함수 코드 생성"
        $             do
                        compileStmt
                          (funcDeclStmt
                            "동작"
                            []
                            (Seq
                              [ Right $ binaryOp Add (litInteger 1) (litInteger 1)
                              , Left $ constDecl "값" (litInteger 10)
                              ]
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              []
                              2
                              1
                              (S.fromList [ConstInteger 1, ConstInteger 10])
                              (ann
                                [ Inst.Push 0
                                , Inst.Push 0
                                , Inst.Add
                                , Inst.Pop
                                , Inst.Push 1
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, storeGlobal 0]
                      )
      it "3개 이상의 스코프가 중첩된 클로저 코드 생성"
        $             do
                        compileStmt
                          (funcDeclStmt
                            "바깥"
                            [("값", "을")]
                            (Seq
                              [ Left $ funcDecl
                                "중간"
                                []
                                (Seq [Left $ funcDecl "안쪽" [] (var "값"), Right $ var "안쪽"])
                              , Right $ var "중간"
                              ]
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              ["을"]
                              1
                              2
                              (S.fromList
                                [ ConstFunc
                                    (FuncObject
                                      []
                                      1
                                      1
                                      (S.fromList
                                        [ ConstFunc
                                            (FuncObject
                                              []
                                              1
                                              0
                                              S.empty
                                              (ann [Inst.LoadDeref 0])
                                            )
                                        ]
                                      )
                                      (ann
                                        [ Inst.Push 0
                                        , Inst.FreeVar [(True, 0)]
                                        , Inst.Store 0
                                        , Inst.Load 0
                                        ]
                                      )
                                    )
                                ]
                              )
                              (ann
                                [ Inst.Push 0
                                , Inst.FreeVar [(False, 0)]
                                , Inst.Store 1
                                , Inst.Load 1
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, Inst.StoreGlobal 1]
                      )
      it "로컬에 선언된 함수가 재귀하는 코드 생성"
        $             do
                        compileStmt
                          (funcDeclStmt
                            "동작"
                            []
                            (Seq
                              [ Left $ funcDecl "재귀" [] (funcCall (var "재귀") [])
                              , Right $ funcCall (var "재귀") []
                              ]
                            )
                          )
        `shouldBuild` ( S.fromList
                        [ ConstFunc
                            (FuncObject
                              []
                              1
                              1
                              (S.fromList
                                [ ConstFunc
                                    (FuncObject
                                      []
                                      1
                                      0
                                      (S.empty)
                                      (ann [Inst.LoadDeref 0, Inst.Call []])
                                    )
                                ]
                              )
                              (ann
                                [ Inst.Push 0
                                , Inst.FreeVar [(False, 0)]
                                , Inst.Store 0
                                , Inst.Load 0
                                , Inst.Call []
                                ]
                              )
                            )
                        ]
                      , [Inst.Push 0, Inst.StoreGlobal 1]
                      )
      -- it "로컬에 선언된 함수 두 개가 상호 재귀하는 코드 생성"
      --   $             do
      --                   compileStmt
      --                     (funcDeclStmt
      --                       "동작"
      --                       []
      --                       (Seq
      --                         [ Left $ funcDecl "재귀1" [] (funcCall (var "재귀2") [])
      --                         , Left $ funcDecl "재귀2" [] (funcCall (var "재귀1") [])
      --                         , Right $ funcCall (var "재귀1") []
      --                         ]
      --                       )
      --                     )
      --   `shouldBuild` ( S.fromList
      --                   [ ConstFunc
      --                       (FuncObject
      --                         []
      --                         (ann
      --                           [ Inst.Push 0
      --                           , Inst.FreeVarLocal 1
      --                           , Inst.Store
      --                           , Inst.Push 0
      --                           , Inst.FreeVarLocal 0
      --                           , Inst.Store
      --                           , Inst.Load 0
      --                           , Inst.Call []
      --                           ]
      --                         )
      --                         (S.fromList
      --                           [ ConstFunc
      --                               (FuncObject
      --                                 []
      --                                 (ann [Inst.LoadDeref 0, Inst.Call []])
      --                                 (S.empty)
      --                               )
      --                           ]
      --                         )
      --                       )
      --                   ]
      --                 , [Inst.Push 0, Inst.StoreGlobal 1]
      --                 )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문 코드 생성" $ do
      compileStmt (constDeclStmt "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1]
                      , [Inst.Push 0, storeGlobal 0]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileStmt
          (constDeclStmt "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2]
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, storeGlobal 0]
                      )

