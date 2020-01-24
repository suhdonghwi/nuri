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
  describe "표현식 구문 코드 생성" $ do
    it "일반 연산 표현식 구문" $ do
      compileStmt (ExprStmt (litInteger 10))
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.Push 1, Inst.Pop]
                      )
    it "1개 이상의 일반 연산 표현식 구문"
      $             (do
                      compileStmt (ExprStmt (litInteger 10))
                      compileStmt (ExprStmt (binaryOp Add (litInteger 20) (litInteger 30)))
                    )
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstInteger 10
                      , ConstInteger 20
                      , ConstInteger 30
                      ]
                    , [ Inst.Push 1
                      , Inst.Pop
                      , Inst.Push 2
                      , Inst.Push 3
                      , Inst.Add
                      , Inst.Pop
                      ]
                    )
  describe "대입 구문 코드 생성" $ do
    it "정수 대입 코드 생성" $ do
      compileStmt (assign "값" (litInteger 10))
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10]
                      , [Inst.Push 1, Inst.StoreGlobal "값"]
                      )
    it "연산식 대입 코드 생성" $ do
      compileStmt (assign "값" (binaryOp Add (litInteger 10) (litInteger 20)))
        `shouldBuild` ( S.fromList [ConstNone, ConstInteger 10, ConstInteger 20]
                      , [ Inst.Push 1
                        , Inst.Push 2
                        , Inst.Add
                        , Inst.StoreGlobal "값"
                        ]
                      )
  describe "함수 선언 코드 생성" $ do
    it "상수 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (litInteger 1)])
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstFunc
                          (FuncObject
                            { _funcArity      = 1
                            , _funcBody       =
                              ann
                                [ Inst.Push 1
                                , Inst.Return
                                , Inst.Push 0
                                , Inst.Return
                                ]
                            , _funcConstTable = S.fromList
                                                  [ConstNone, ConstInteger 1]
                            }
                          )
                        ]
                      , [Inst.Push 1, Inst.StoreGlobal "더하다"]
                      )
    it "항등 함수 코드 생성" $ do
      compileStmt (funcDecl "더하다" ["값"] [Return (var "값")])
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstFunc
                          (FuncObject
                            { _funcArity      = 1
                            , _funcBody       =
                              ann
                                [ Inst.Load 0
                                , Inst.Return
                                , Inst.Push 0
                                , Inst.Return
                                ]
                            , _funcConstTable = S.singleton ConstNone
                            }
                          )
                        ]
                      , [Inst.Push 1, Inst.StoreGlobal "더하다"]
                      )
    it "외부 변수가 존재할 때 함수 코드 생성"
      $             (do
                      compileStmt (assign "수" (litInteger 10))
                      compileStmt (funcDecl "더하다" ["값"] [Return (var "값")])
                      compileStmt (ExprStmt $ funcCall "더하다" [litInteger 10])
                    )
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstInteger 10
                      , ConstFunc
                        (FuncObject
                          { _funcArity      = 1
                          , _funcBody       =
                            ann
                              [ Inst.Load 0
                              , Inst.Return
                              , Inst.Push 0
                              , Inst.Return
                              ]
                          , _funcConstTable = S.singleton ConstNone
                          }
                        )
                      ]
                    , [ Inst.Push 1
                      , Inst.StoreGlobal "수"
                      , Inst.Push 2
                      , Inst.StoreGlobal "더하다"
                      , Inst.LoadGlobal "더하다"
                      , Inst.Push 1
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
        `shouldBuild` ( S.fromList [ConstNone, ConstBool True, ConstInteger 10]
                      , [ Inst.Push 1
                        , Inst.PopJmpIfFalse $ Value 6
                        , Inst.LoadGlobal "던지다"
                        , Inst.Push 2
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
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstBool False
                        , ConstInteger 10
                        , ConstInteger 5
                        ]
                      , [ Inst.Push 1
                        , Inst.PopJmpIfFalse $ Value 7
                        , Inst.LoadGlobal "던지다"
                        , Inst.Push 2
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.Jmp $ Value 11
                        , Inst.LoadGlobal "밟다"
                        , Inst.Push 3
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
        `shouldBuild` ( S.fromList
                        [ ConstNone
                        , ConstBool True
                        , ConstInteger 10
                        , ConstInteger 20
                        ]
                      , [ Inst.Push 1
                        , Inst.PopJmpIfFalse $ Value 9
                        , Inst.Push 2
                        , Inst.StoreGlobal "값"
                        , Inst.LoadGlobal "던지다"
                        , Inst.LoadGlobal "값"
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.Jmp $ Value 11
                        , Inst.Push 3
                        , Inst.StoreGlobal "값2"
                        ]
                      )
    it "같은 이름의 외부 변수가 존재할 때 조건문 코드 생성"
      $             (do
                      compileStmt (assign "값" (litInteger 0))
                      compileStmt
                        (ifStmt (litBool True) [assign "값" (litInteger 10)] Nothing)
                    )
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstInteger 0
                      , ConstBool True
                      , ConstInteger 10
                      ]
                    , [ Inst.Push 1
                      , Inst.StoreGlobal "값"
                      , Inst.Push 2
                      , Inst.PopJmpIfFalse $ Value 6
                      , Inst.Push 3
                      , Inst.StoreGlobal "값"
                      ]
                    )
  describe "반복문 코드 생성" $ do
    it "무한 반복문 코드 생성" $ do
      compileStmt
          (while (litBool True) [ExprStmt $ funcCall "보여주다" [litString "안녕하세요"]]
          )
        `shouldBuild` ( S.fromList
                        [ConstNone, ConstBool True, ConstString "안녕하세요"]
                      , [ Inst.Push 1
                        , Inst.PopJmpIfFalse $ Value 7
                        , Inst.LoadGlobal "보여주다"
                        , Inst.Push 2
                        , Inst.Call 1
                        , Inst.Pop
                        , Inst.Jmp $ Value 0
                        ]
                      )
    it "증감하는 반복문 코드 생성"
      $             (do
                      compileStmt (assign "값" (litInteger 0))
                      compileStmt
                        (while
                          (binaryOp LessThan (var "값") (litInteger 10))
                          [ ExprStmt $ funcCall "보여주다" [var "값"]
                          , assign "값" (binaryOp Add (var "값") (litInteger 1))
                          ]
                        )
                    )
      `shouldBuild` ( S.fromList
                      [ ConstNone
                      , ConstInteger 0
                      , ConstInteger 10
                      , ConstInteger 1
                      ]
                    , [ Inst.Push 1
                      , Inst.StoreGlobal "값"
                      , Inst.LoadGlobal "값"
                      , Inst.Push 2
                      , Inst.LessThan
                      , Inst.PopJmpIfFalse $ Value 15
                      , Inst.LoadGlobal "보여주다"
                      , Inst.LoadGlobal "값"
                      , Inst.Call 1
                      , Inst.Pop
                      , Inst.LoadGlobal "값"
                      , Inst.Push 3
                      , Inst.Add
                      , Inst.StoreGlobal "값"
                      , Inst.Jmp $ Value 2
                      ]
                    )

