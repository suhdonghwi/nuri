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
    it "연산식 대입 코드 생성" $ do
      compileStmt (assign "값" (binaryOp Add (litInteger 10) (litInteger 20)))
        `shouldBuild` ( defaultI
                        { _constTable = S.fromList
                                          [ConstInteger 10, ConstInteger 20]
                        , _varNames   = S.singleton "값"
                        }
                      , [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.Store 0]
                      )
  describe "함수 선언 코드 생성" $ do
    it "상수 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (litInteger 1)])
        `shouldBuild` ( defaultI
                        { _constTable =
                          S.singleton $ ConstFunc
                            (FuncObject
                              { _arity          = 1
                              , _insts = [ann (Inst.Push 0), ann Inst.Return]
                              , _funcConstTable = S.singleton (ConstInteger 1)
                              , _funcVarNames   = S.singleton "값"
                              }
                            )
                        , _varNames   = S.singleton "더하다"
                        }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "항등 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (var "값")])
        `shouldBuild` ( defaultI
                        { _constTable = S.singleton $ ConstFunc
                                          (FuncObject
                                            { _arity          = 1
                                            , _insts = [ ann (Inst.Load 0)
                                                       , ann Inst.Return
                                                       ]
                                            , _funcConstTable = S.empty
                                            , _funcVarNames   = S.singleton "값"
                                            }
                                          )
                        , _varNames   = S.singleton "더하다"
                        }
                      , [Inst.Push 0, Inst.Store 0]
                      )
    it "외부 변수가 존재할 때 함수 코드 생성"
      $             (do
                      compileStmt (assign "수" (litInteger 10))
                      compileStmt (funcDecl "더하다" ["값"] [Return (litInteger 20)])
                    )
      `shouldBuild` ( defaultI
                      { _constTable =
                        S.fromList
                          [ ConstInteger 10
                          , ConstFunc
                            (FuncObject
                              { _arity          = 1
                              , _insts = [ann (Inst.Push 0), ann Inst.Return]
                              , _funcConstTable = S.fromList [ConstInteger 20]
                              , _funcVarNames   = S.fromList ["값"]
                              }
                            )
                          ]
                      , _varNames   = S.fromList ["수", "더하다"]
                      }
                    , [Inst.Push 0, Inst.Store 0, Inst.Push 1, Inst.Store 1]
                    )
  describe "조건문 코드 생성" $ do
    it "else문이 없는 조건문 코드 생성" $ do
      compileStmt
          (ifStmt (litBool True)
                  [ExprStmt $ funcCall "던지다" [litInteger 10]]
                  Nothing
          )
        `shouldBuild` ( defaultI
                        { _constTable = S.fromList
                                          [ConstBool True, ConstInteger 10]
                        , _varNames   = S.singleton "던지다"
                        }
                      , [ Inst.Push 0
                        , Inst.PopJmpIfFalse 13
                        , Inst.Load 0
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
                        { _constTable =
                          S.fromList
                            [ConstBool False, ConstInteger 10, ConstInteger 5]
                        , _varNames   = S.fromList ["던지다", "밟다"]
                        }
                      , [ Inst.Push 0
                        , Inst.PopJmpIfFalse 17
                        , Inst.Load 0
                        , Inst.Push 1
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.JmpForward 13
                        , Inst.Load 1
                        , Inst.Push 2
                        , Inst.Call 1
                        , Inst.Pop
                        ]
                      )

