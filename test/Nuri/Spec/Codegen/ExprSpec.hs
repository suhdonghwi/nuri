module Nuri.Spec.Codegen.ExprSpec where

import           Test.Hspec

import qualified Data.Set.Ordered              as S

import           Nuri.Spec.Util
import           Nuri.Spec.Codegen.Util

import           Nuri.Expr
import           Nuri.Codegen.Expr

import qualified Haneul.Instruction            as Inst
import           Haneul.Builder
import           Haneul.Constant

spec :: Spec
spec = do
  describe "리터럴 코드 생성" $ do
    it "정수 리터럴 코드 생성" $ do
      compileExpr (litInteger 10)
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        }
                      , [Inst.Push 0]
                      )
    it "리터럴 여러개 코드 생성"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                    )
      `shouldBuild` ( defaultI
                      { _internalConstTable = S.fromList
                                                [ConstInteger 10, ConstChar 'a']
                      }
                    , [Inst.Push 0, Inst.Push 1]
                    )
    it "리터럴 여러개 코드 생성 (중복 포함)"
      $             (do
                      compileExpr (litInteger 10)
                      compileExpr (litChar 'a')
                      compileExpr (litInteger 10)
                    )
      `shouldBuild` ( defaultI
                      { _internalConstTable = S.fromList
                                                [ConstInteger 10, ConstChar 'a']
                      }
                    , [Inst.Push 0, Inst.Push 1, Inst.Push 0]
                    )
  describe "이항 연산 코드 생성" $ do
    it "덧셈 코드 생성" $ do
      compileExpr (binaryOp Add (litInteger 10) (litInteger 20))
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList [ConstInteger 10, ConstInteger 20]
                        }
                      , [Inst.Push 0, Inst.Push 1, Inst.Add]
                      )
    it "복합 연산 코드 생성" $ do
      compileExpr
          (binaryOp Add
                    (litInteger 10)
                    (binaryOp Divide (litInteger 20) (litInteger 30))
          )
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList
                            [ConstInteger 10, ConstInteger 20, ConstInteger 30]
                        }
                      , [ Inst.Push 0
                        , Inst.Push 1
                        , Inst.Push 2
                        , Inst.Divide
                        , Inst.Add
                        ]
                      )
  describe "단항 연산 코드 생성" $ do
    it "양수 코드 생성" $ do
      compileExpr (unaryOp Positive (litInteger 10))
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        }
                      , [Inst.Push 0]
                      )
    it "음수 코드 생성" $ do
      compileExpr (unaryOp Negative (litInteger 10))
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        }
                      , [Inst.Push 0, Inst.Negate]
                      )
  describe "변수 접근 코드 생성" $ do
    it "선언되지 않은 변수 이름에 대해 Load 코드 생성" $ do
      compileExpr (var "값")
        `shouldBuild` ( defaultI { _internalVarNames = S.singleton ("값", 0) }
                      , [Inst.Load 0]
                      )

  describe "함수 호출 코드 생성" $ do
    it "인수가 하나인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10])
        `shouldBuild` ( defaultI
                        { _internalConstTable = S.singleton (ConstInteger 10)
                        , _internalVarNames   = S.singleton ("던지다", 0)
                        }
                      , [Inst.Load 0, Inst.Push 0, Inst.Call 1]
                      )
    it "인수가 3개인 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [litInteger 10, litInteger 10, litInteger 0])
        `shouldBuild` ( defaultI
                        { _internalConstTable =
                          S.fromList [ConstInteger 10, ConstInteger 0]
                        , _internalVarNames   = S.singleton ("던지다", 0)
                        }
                      , [ Inst.Load 0
                        , Inst.Push 0
                        , Inst.Push 0
                        , Inst.Push 1
                        , Inst.Call 3
                        ]
                      )
    it "인수가 없는 함수 호출 코드 생성" $ do
      compileExpr (funcCall "던지다" [])
        `shouldBuild` ( defaultI { _internalVarNames = S.singleton ("던지다", 0) }
                      , [Inst.Load 0, Inst.Call 0]
                      )

