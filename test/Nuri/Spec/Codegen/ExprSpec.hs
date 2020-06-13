{-# LANGUAGE OverloadedLists #-}

module Nuri.Spec.Codegen.ExprSpec where

import qualified Data.Set.Ordered as S
import Haneul.Constant
import qualified Haneul.Instruction as Inst
import Nuri.Codegen.Expr
import Nuri.Expr
import Nuri.Spec.Codegen.Util
import Nuri.Spec.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "리터럴 코드 생성" $ do
    it "정수 리터럴 코드 생성" $ do
      compileExpr (litInteger 10)
        `shouldBuild` (S.fromList [ConstInteger 10], [Inst.Push 0])
    it "리터럴 여러개 코드 생성" $
      ( do
          compileExpr (litInteger 10)
          compileExpr (litChar 'a')
      )
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstChar 'a'],
                        [Inst.Push 0, Inst.Push 1]
                      )
    it "리터럴 여러개 코드 생성 (중복 포함)" $
      ( do
          compileExpr (litInteger 10)
          compileExpr (litChar 'a')
          compileExpr (litInteger 10)
      )
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstChar 'a'],
                        [Inst.Push 0, Inst.Push 1, Inst.Push 0]
                      )
  describe "조건식 코드 생성" $ do
    it "단순 리터럴에 대한 조건식 코드 생성" $
      compileExpr (ifExpr (litBool True) (litInteger 1) (litInteger 2))
        `shouldBuild` ( S.fromList
                          [ConstBool True, ConstInteger 1, ConstInteger 2],
                        [ Inst.Push 0,
                          Inst.PopJmpIfFalse 4,
                          Inst.Push 1,
                          Inst.Jmp 5,
                          Inst.Push 2
                        ]
                      )
    it "계산식이 포함된 조건식 코드 생성" $
      compileExpr
        ( ifExpr
            (binaryOp Equal (litInteger 1) (litInteger 2))
            (binaryOp Add (litInteger 3) (litInteger 4))
            (litInteger 5)
        )
        `shouldBuild` ( S.fromList
                          [ ConstInteger 1,
                            ConstInteger 2,
                            ConstInteger 3,
                            ConstInteger 4,
                            ConstInteger 5
                          ],
                        [ Inst.Push 0,
                          Inst.Push 1,
                          Inst.Equal,
                          Inst.PopJmpIfFalse 8,
                          Inst.Push 2,
                          Inst.Push 3,
                          Inst.Add,
                          Inst.Jmp 9,
                          Inst.Push 4
                        ]
                      )
  describe "이항 연산 코드 생성" $ do
    it "덧셈 코드 생성" $ do
      compileExpr (binaryOp Add (litInteger 10) (litInteger 20))
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstInteger 20],
                        [Inst.Push 0, Inst.Push 1, Inst.Add]
                      )
    it "복합 연산 코드 생성" $ do
      compileExpr
        ( binaryOp
            Add
            (litInteger 10)
            (binaryOp Divide (litInteger 20) (litInteger 30))
        )
        `shouldBuild` ( S.fromList
                          [ConstInteger 10, ConstInteger 20, ConstInteger 30],
                        [ Inst.Push 0,
                          Inst.Push 1,
                          Inst.Push 2,
                          Inst.Divide,
                          Inst.Add
                        ]
                      )
  describe "단항 연산 코드 생성" $ do
    it "양수 코드 생성" $ do
      compileExpr (unaryOp Positive (litInteger 10))
        `shouldBuild` (S.fromList [ConstInteger 10], [Inst.Push 0])
    it "음수 코드 생성" $ do
      compileExpr (unaryOp Negative (litInteger 10))
        `shouldBuild` (S.fromList [ConstInteger 10], [Inst.Push 0, Inst.Negate])
    it "논리 부정 코드 생성" $ do
      compileExpr (unaryOp LogicNot (litBool False))
        `shouldBuild` (S.fromList [ConstBool False], [Inst.Push 0, Inst.LogicNot])

  describe "변수 접근 코드 생성" $ do
    it "변수 이름 접근하는 LoadGlobal 코드 생성" $ do
      compileExpr (var "값") `shouldBuild` (S.empty, [Inst.LoadGlobal 0])

  describe "함수 호출 코드 생성" $ do
    it "인수가 하나인 함수 호출 코드 생성" $ do
      compileExpr (funcCall (var "던지다") [(litInteger 10, "을")])
        `shouldBuild` ( S.fromList [ConstInteger 10],
                        [Inst.Push 0, Inst.LoadGlobal 0, Inst.Call ["을"]]
                      )
    it "인수가 3개인 함수 호출 코드 생성" $ do
      compileExpr
        ( funcCall
            (var "던지다")
            [(litInteger 10, "와"), (litInteger 10, "을"), (litInteger 0, "에")]
        )
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstInteger 0],
                        [ Inst.Push 0,
                          Inst.Push 0,
                          Inst.Push 1,
                          Inst.LoadGlobal 0,
                          Inst.Call ["에", "을", "와"]
                        ]
                      )
    it "인수가 없는 함수 호출 코드 생성" $ do
      compileExpr (funcCall (var "던지다") [])
        `shouldBuild` (S.empty, [Inst.LoadGlobal 0, Inst.Call []])
  describe "표현식 시퀀스 코드 생성" $ do
    it "표현식이 1개인 시퀀스 코드 생성" $ do
      compileExpr (Seq [Right $ litInteger 10])
        `shouldBuild` (S.fromList [ConstInteger 10], [Inst.Push 0])
    it "표현식이 2개인 시퀀스 코드 생성" $ do
      compileExpr (Seq [Right $ litInteger 10, Right $ litBool True])
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstBool True],
                        [Inst.Push 0, Inst.Pop, Inst.Push 1]
                      )

  describe "람다 코드 생성" $ do
    it "인수가 하나인 람다 코드 생성" $ do
      compileExpr (lambda [("값", "을")] (binaryOp Add (var "값") (litInteger 1)))
        `shouldBuild` ( S.fromList
                          [ ConstFunc
                              ( funcObject
                                  { _funcJosa = ["을"],
                                    _funcStackSize = 2,
                                    _funcConstTable = S.fromList [ConstInteger 1],
                                    _funcCode =
                                      ann
                                        [Inst.LoadLocal 0, Inst.Push 0, Inst.Add]
                                  }
                              )
                          ],
                        [Inst.Push 0]
                      )
    it "인수가 두 개인 람다 코드 생성" $ do
      compileExpr
        ( lambda
            [("수1", "에"), ("수2", "를")]
            (binaryOp Add (var "수1") (var "수2"))
        )
        `shouldBuild` ( S.fromList
                          [ ConstFunc
                              ( funcObject
                                  { _funcJosa = ["에", "를"],
                                    _funcStackSize = 2,
                                    _funcCode =
                                      ann
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Add]
                                  }
                              )
                          ],
                        [Inst.Push 0]
                      )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문 코드 생성" $ do
      compileExpr
        ( Seq
            [ Left $ constDecl "값" (litInteger 1),
              Right $ binaryOp Add (var "값") (litInteger 2)
            ]
        )
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2],
                        [ Inst.Push 0,
                          Inst.StoreLocal 0,
                          Inst.LoadLocal 0,
                          Inst.Push 1,
                          Inst.Add
                        ]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileExpr
        ( Seq
            [ Left $ constDecl "값" (binaryOp Add (litInteger 1) (litInteger 2)),
              Right $ binaryOp Add (var "값") (litInteger 3)
            ]
        )
        `shouldBuild` ( S.fromList
                          [ConstInteger 1, ConstInteger 2, ConstInteger 3],
                        [ Inst.Push 0,
                          Inst.Push 1,
                          Inst.Add,
                          Inst.StoreLocal 0,
                          Inst.LoadLocal 0,
                          Inst.Push 2,
                          Inst.Add
                        ]
                      )
    it "현재 스코프에 없는 상수는 전역 상수로 취급" $ do
      compileExpr
        ( Seq
            [ Right $ Seq [Left $ constDecl "값" (litInteger 5), Right $ var "값"],
              Right $ binaryOp Add (var "값") (litInteger 3)
            ]
        )
        `shouldBuild` ( S.fromList [ConstInteger 5, ConstInteger 3],
                        [ Inst.Push 0,
                          Inst.StoreLocal 0,
                          Inst.LoadLocal 0,
                          Inst.Pop,
                          Inst.LoadGlobal 0,
                          Inst.Push 1,
                          Inst.Add
                        ]
                      )
    it "같은 이름의 상수는 섀도잉 적용" $ do
      compileExpr
        ( Seq
            [ Left $ constDecl "값" (litInteger 10),
              Left $ constDecl "값" (litInteger 5),
              Right $ var "값"
            ]
        )
        `shouldBuild` ( S.fromList [ConstInteger 10, ConstInteger 5],
                        [ Inst.Push 0,
                          Inst.StoreLocal 0,
                          Inst.Push 1,
                          Inst.StoreLocal 0,
                          Inst.LoadLocal 0
                        ]
                      )
      compileExpr
        ( Seq
            [ Left $ constDecl "값" (litInteger 10),
              Right $ Seq [Left $ constDecl "값" (litInteger 5), Right $ var "값"],
              Right $ binaryOp Add (var "값") (litInteger 3)
            ]
        )
        `shouldBuild` ( S.fromList
                          [ConstInteger 10, ConstInteger 5, ConstInteger 3],
                        [ Inst.Push 0,
                          Inst.StoreLocal 0,
                          Inst.Push 1,
                          Inst.StoreLocal 1,
                          Inst.LoadLocal 1,
                          Inst.Pop,
                          Inst.LoadLocal 0,
                          Inst.Push 2,
                          Inst.Add
                        ]
                      )
