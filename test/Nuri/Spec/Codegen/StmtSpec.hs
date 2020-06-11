{-# LANGUAGE OverloadedLists #-}

module Nuri.Spec.Codegen.StmtSpec where

import qualified Data.Set.Ordered as S
import Haneul.Constant
import qualified Haneul.Instruction as Inst
import Nuri.Codegen.Stmt
import Nuri.Expr
import Nuri.Spec.Codegen.Util
import Nuri.Spec.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "선언문 코드 생성" $ do
    describe "함수 선언 코드 생성" $ do
      it "인자가 하나인 함수 선언 코드 생성" $
        compileStmt
          ( funcDeclStmt
              NormalDecl
              "더하다"
              [("값", "을")]
              (binaryOp Add (var "값") (litInteger 1))
          )
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
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "인자가 두 개인 함수 선언 코드 생성" $
        compileStmt
          ( funcDeclStmt
              NormalDecl
              "더하다"
              [("수1", "에"), ("수2", "을")]
              (binaryOp Add (var "수1") (var "수2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["에", "을"],
                                      _funcStackSize = 2,
                                      _funcCode =
                                        ann
                                          [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Add]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "외부에 전역 변수가 있는 함수 선언 코드 생성" $
        do
          compileStmt (constDeclStmt NormalDecl "값" (litInteger 1))
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "더하다"
                [("수", "을")]
                (binaryOp Add (var "수") (var "값"))
            )
          `shouldBuild` ( S.fromList
                            [ ConstInteger 1,
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["을"],
                                      _funcGlobalVarNames = S.fromList ["값"],
                                      _funcStackSize = 2,
                                      _funcCode =
                                        ann
                                          [Inst.LoadLocal 0, Inst.LoadGlobal 0, Inst.Add]
                                    }
                                )
                            ],
                          [ Inst.Push 0,
                            Inst.StoreGlobal 0,
                            Inst.Push 1,
                            Inst.StoreGlobal 1
                          ]
                        )
      it "외부에 전역 변수가 있을 때 변수 섀도잉 하는 함수 선언 코드 생성" $
        do
          compileStmt (constDeclStmt NormalDecl "값" (litInteger 1))
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "더하다"
                [("값", "을")]
                (binaryOp Add (var "값") (litInteger 2))
            )
          `shouldBuild` ( S.fromList
                            [ ConstInteger 1,
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["을"],
                                      _funcStackSize = 2,
                                      _funcConstTable = S.singleton (ConstInteger 2),
                                      _funcCode =
                                        ann
                                          [Inst.LoadLocal 0, Inst.Push 0, Inst.Add]
                                    }
                                )
                            ],
                          [ Inst.Push 0,
                            Inst.StoreGlobal 0,
                            Inst.Push 1,
                            Inst.StoreGlobal 1
                          ]
                        )
      it "함수의 인자를 참조하는 함수 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "더하다"
                [("값1", "을")]
                ( Seq
                    [ Left $ constDecl NormalDecl "값2" (litInteger 1),
                      Right $ binaryOp Add (var "값1") (var "값2")
                    ]
                )
            )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["을"],
                                      _funcStackSize = 2,
                                      _funcMaxLocalCount = 2,
                                      _funcConstTable = S.fromList [ConstInteger 1],
                                      _funcCode =
                                        ann
                                          [ Inst.Push 0,
                                            Inst.StoreLocal 1,
                                            Inst.LoadLocal 0,
                                            Inst.LoadLocal 1,
                                            Inst.Add
                                          ]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "시퀀스의 마지막이 선언 문인 함수 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "동작"
                []
                ( Seq
                    [ Right $ binaryOp Add (litInteger 1) (litInteger 1),
                      Left $ constDecl NormalDecl "값" (litInteger 10)
                    ]
                )
            )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcStackSize = 2,
                                      _funcMaxLocalCount = 1,
                                      _funcConstTable =
                                        S.fromList
                                          [ConstInteger 1, ConstInteger 10],
                                      _funcCode =
                                        ann
                                          [ Inst.Push 0,
                                            Inst.Push 0,
                                            Inst.Add,
                                            Inst.Pop,
                                            Inst.Push 1,
                                            Inst.StoreLocal 0,
                                            Inst.LoadLocal 0
                                          ]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "3개 이상의 스코프가 중첩된 클로저 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "바깥"
                [("값", "을")]
                ( Seq
                    [ Left $
                        funcDecl
                          NormalDecl
                          "중간"
                          []
                          (Seq [Left $ funcDecl NormalDecl "안쪽" [] (var "값"), Right $ var "안쪽"]),
                      Right $ var "중간"
                    ]
                )
            )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["을"],
                                      _funcStackSize = 1,
                                      _funcMaxLocalCount = 2,
                                      _funcConstTable =
                                        ( S.fromList
                                            [ ConstFunc
                                                ( funcObject
                                                    { _funcStackSize = 1,
                                                      _funcMaxLocalCount = 1,
                                                      _funcConstTable =
                                                        S.fromList
                                                          [ ConstFunc
                                                              ( funcObject
                                                                  { _funcStackSize = 1,
                                                                    _funcCode =
                                                                      ann
                                                                        [Inst.LoadDeref 0]
                                                                  }
                                                              )
                                                          ],
                                                      _funcCode =
                                                        ann
                                                          [ Inst.Push 0,
                                                            Inst.FreeVar [(True, 0)],
                                                            Inst.StoreLocal 0,
                                                            Inst.LoadLocal 0
                                                          ]
                                                    }
                                                )
                                            ]
                                        ),
                                      _funcCode =
                                        ann
                                          [ Inst.Push 0,
                                            Inst.FreeVar [(False, 0)],
                                            Inst.StoreLocal 1,
                                            Inst.LoadLocal 1
                                          ]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "로컬에 선언된 함수가 재귀하는 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                NormalDecl
                "동작"
                []
                ( Seq
                    [ Left $ funcDecl NormalDecl "재귀" [] (funcCall (var "재귀") []),
                      Right $ funcCall (var "재귀") []
                    ]
                )
            )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcStackSize = 1,
                                      _funcMaxLocalCount = 1,
                                      _funcConstTable =
                                        S.fromList
                                          [ ConstFunc
                                              ( funcObject
                                                  { _funcStackSize = 1,
                                                    _funcCode =
                                                      ann
                                                        [Inst.LoadDeref 0, Inst.Call []]
                                                  }
                                              )
                                          ],
                                      _funcCode =
                                        ann
                                          [ Inst.Push 0,
                                            Inst.FreeVar [(False, 0)],
                                            Inst.StoreLocal 0,
                                            Inst.LoadLocal 0,
                                            Inst.Call []
                                          ]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
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
  --                           , Inst.LoadLocal 0
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
      compileStmt (constDeclStmt NormalDecl "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1],
                        [Inst.Push 0, Inst.StoreGlobal 0]
                      )
    it "계산식 상수 선언문 코드 생성" $ do
      compileStmt
        (constDeclStmt NormalDecl "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2],
                        [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.StoreGlobal 0]
                      )
