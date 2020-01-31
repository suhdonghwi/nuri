{-# LANGUAGE OverloadedLists #-}
module Nuri.Spec.Codegen.StmtSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Codegen.Stmt

import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( Marked(Value) )
import           Haneul.Constant


spec :: Spec
spec = do
  describe "함수 선언 코드 생성" $ do
    it "상수 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] (litInteger 1))
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstFunc
                          (FuncObject
                            { _funcArity      = 1
                            , _funcBody       = ann [Inst.Push 1]
                            , _funcConstTable = S.fromList
                                                  [ConstNone, ConstInteger 1]
                            }
                          )
                        ]
                      , [Inst.Push 1, Inst.StoreGlobal "더하다"]
                      )
    it "항등 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] (var "값"))
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstFunc
                          (FuncObject { _funcArity      = 1
                                      , _funcBody       = ann [Inst.Load 0]
                                      , _funcConstTable = S.singleton ConstNone
                                      }
                          )
                        ]
                      , [Inst.Push 1, Inst.StoreGlobal "더하다"]
                      )
    it "시퀀스가 포함된 함수 코드 생성" $ do
      compileStmt
          (funcDecl
            "더하다"
            ["값"]
            (Seq
              [ funcCall "보여주다" [litInteger 1]
              , binaryOp Add (litInteger 1) (litInteger 2)
              ]
            )
          )
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstFunc
                          (FuncObject
                            { _funcArity      = 1
                            , _funcBody       = ann
                                                  [ Inst.LoadGlobal "보여주다"
                                                  , Inst.Push 1
                                                  , Inst.Call 1
                                                  , Inst.Pop
                                                  , Inst.Push 1
                                                  , Inst.Push 2
                                                  , Inst.Add
                                                  ]
                            , _funcConstTable = S.fromList
                              [ConstNone, ConstInteger 1, ConstInteger 2]
                            }
                          )
                        ]
                      , [Inst.Push 1, Inst.StoreGlobal "더하다"]
                      )
