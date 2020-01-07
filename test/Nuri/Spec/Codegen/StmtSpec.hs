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
import           Haneul.Builder
import           Haneul.Constant

spec :: Spec
spec = do
  describe "표현식 구문 코드 생성" $ do
    it "일반 연산 표현식 구문" $ do
      compileStmt (ExprStmt (litInteger 10))
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        }
                      , [Inst.Push 0, Inst.Pop]
                      )
    it "1개 이상의 일반 연산 표현식 구문"
      $             (do
                      compileStmt (ExprStmt (litInteger 10))
                      compileStmt (ExprStmt (binaryOp Add (litInteger 20) (litInteger 30)))
                    )
      `shouldBuild` ( defaultI
                      { _internalConstTable =
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
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        , _internalVarNames   = S.singleton ("값", 0)
                        }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "연산식 대입 코드 생성" $ do
      compileStmt (assign "값" (binaryOp Add (litInteger 10) (litInteger 20)))
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList [ConstInteger 10, ConstInteger 20]
                        , _internalVarNames   = S.singleton ("값", 0)
                        }
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.Store 0]
                      )
  describe "함수 선언 코드 생성" $ do
    it "상수 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (litInteger 1)])
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.singleton $ ConstFunc
                            (FuncObject
                              { _funcArity      = 1
                              , _funcBody       = ann [Inst.Push 0, Inst.Return]
                              , _funcConstTable = S.singleton (ConstInteger 1)
                              , _funcVarNames   = S.fromList
                                                    [("더하다", 0), ("값", 1)]
                              }
                            )
                        , _internalVarNames   = S.singleton ("더하다", 0)
                        }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "항등 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (var "값")])
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.singleton $ ConstFunc
                            (FuncObject
                              { _funcArity      = 1
                              , _funcBody       = ann [Inst.Load 1, Inst.Return]
                              , _funcConstTable = S.empty
                              , _funcVarNames   = S.fromList
                                                    [("더하다", 0), ("값", 1)]
                              }
                            )
                        , _internalVarNames   = S.singleton ("더하다", 0)
                        }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "외부 변수가 존재할 때 함수 코드 생성"
      $             (do
                      compileStmt (assign "수" (litInteger 10))
                      compileStmt (funcDecl "더하다" ["값"] [Return (litInteger 20)])
                      compileStmt (ExprStmt $ funcCall "더하다" [litInteger 10])
                    )
      `shouldBuild` ( defaultI
                      { _internalConstTable =
                        S.fromList
                          [ ConstInteger 10
                          , ConstFunc
                            (FuncObject
                              { _funcArity      = 1
                              , _funcBody       = ann [Inst.Push 0, Inst.Return]
                              , _funcConstTable = S.fromList [ConstInteger 20]
                              , _funcVarNames   =
                                S.fromList [("수", 0), ("더하다", 0), ("값", 1)]
                              }
                            )
                          ]
                      , _internalVarNames   = S.fromList [("수", 0), ("더하다", 0)]
                      }
                    , [ Inst.Push 0
                      , Inst.Store 0
                      , Inst.Push 1
                      , Inst.Store 1
                      , Inst.Load 1
                      , Inst.Push 0
                      , Inst.Call 1
                      , Inst.Pop
                      ]
                    )
  describe "조건문 코드 생성" $ do
    it "else문이 없는 조건문 코드 생성" $ do
      compileStmt
          (ifStmt (litBool True)
                  [ExprStmt $ funcCall "던지다" [litInteger 10]]
                  Nothing
          )
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList [ConstBool True, ConstInteger 10]
                        , _internalVarNames   = S.empty
                        }
                      , [ Inst.Push 0
                        , Inst.PopJmpIfFalse 4
                        , Inst.LoadGlobal "던지다"
                        , Inst.Push 1
                        , Inst.Call 1
                        , Inst.Pop
                        ]
                      )
    it "else문이 있는 조건문 코드 생성" $ do
      compileStmt
          (ifStmt (litBool False)
                  [ExprStmt $ funcCall "던지다" [litInteger 10]]
                  (Just [ExprStmt $ funcCall "밟다" [litInteger 5]])
          )
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList
                            [ConstBool False, ConstInteger 10, ConstInteger 5]
                        , _internalVarNames   = S.empty
                        }
                      , [ Inst.Push 0
                        , Inst.PopJmpIfFalse 5
                        , Inst.LoadGlobal "던지다"
                        , Inst.Push 1
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.JmpForward 4
                        , Inst.LoadGlobal "밟다"
                        , Inst.Push 2
                        , Inst.Call 1
                        , Inst.Pop
                        ]
                      )
    it "조건문 스코프 코드 생성" $ do
      compileStmt
          (ifStmt
            (litBool True)
            [assign "값" (litInteger 10), ExprStmt $ funcCall "던지다" [var "값"]]
            (Just [assign "값2" (litInteger 20)])
          )
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList
                            [ConstBool True, ConstInteger 10, ConstInteger 20]
                        , _internalVarNames   = S.empty
                        }
                      , [ Inst.Push 0
                        , Inst.PopJmpIfFalse 7
                        , Inst.Push 1
                        , Inst.Store 0
                        , Inst.LoadGlobal "던지다"
                        , Inst.Load 0
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.JmpForward 2
                        , Inst.Push 2
                        , Inst.Store 0
                        ]
                      )

