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
                                      _funcName = "더하다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.Push 0, Inst.Add]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "인자가 두 개인 함수 선언 코드 생성" $
        compileStmt
          ( funcDeclStmt
              "더하다"
              [("수1", "에"), ("수2", "을")]
              (binaryOp Add (var "수1") (var "수2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["에", "을"],
                                      _funcStackSize = 2,
                                      _funcName = "더하다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Add]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "외부에 전역 변수가 있는 함수 선언 코드 생성" $
        do
          compileStmt (constDeclStmt "값" (litInteger 1))
          compileStmt
            ( funcDeclStmt
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
                                      _funcName = "더하다",
                                      _funcCode =
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
          compileStmt (constDeclStmt "값" (litInteger 1))
          compileStmt
            ( funcDeclStmt
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
                                      _funcName = "더하다",
                                      _funcCode =
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
                "더하다"
                [("값1", "을")]
                ( Seq
                    [ Left $ constDecl "값2" (litInteger 1),
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
                                      _funcName = "더하다",
                                      _funcCode =
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
      it "3개 이상의 스코프가 중첩된 클로저 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                "바깥"
                [("값", "을")]
                ( Seq
                    [ Left $
                        funcDecl
                          "중간"
                          []
                          (Seq [Left $ funcDecl "안쪽" [] (var "값"), Right $ var "안쪽"]),
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
                                                                    _funcName = "안쪽",
                                                                    _funcCode =
                                                                      [Inst.LoadDeref 0]
                                                                  }
                                                              )
                                                          ],
                                                      _funcName = "중간",
                                                      _funcCode =
                                                        [ Inst.Push 0,
                                                          Inst.FreeVar [(True, 0)],
                                                          Inst.StoreLocal 0,
                                                          Inst.LoadLocal 0
                                                        ]
                                                    }
                                                )
                                            ]
                                        ),
                                      _funcName = "바깥",
                                      _funcCode =
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
                "동작"
                []
                ( Seq
                    [ Left $ funcDecl "재귀" [] (funcCall (var "재귀") []),
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
                                                    _funcName = "재귀",
                                                    _funcCode =
                                                      [Inst.LoadDeref 0, Inst.Call []]
                                                  }
                                              )
                                          ],
                                      _funcName = "동작",
                                      _funcCode =
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
      it "로컬에 선언된 함수 두 개가 상호 재귀하는 코드 생성" $
        do
          compileStmt
            ( funcDeclStmt
                "동작"
                []
                ( Seq
                    [ Left $ funcForward "재귀2" [],
                      Left $ funcDecl "재귀1" [] (funcCall (var "재귀2") []),
                      Left $ funcDecl "재귀2" [] (funcCall (var "재귀1") []),
                      Right $ funcCall (var "재귀1") []
                    ]
                )
            )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcStackSize = 1,
                                      _funcMaxLocalCount = 2,
                                      _funcCode =
                                        ( [ Inst.Push 0,
                                            Inst.FreeVar [(False, 0)],
                                            Inst.StoreLocal 1,
                                            Inst.Push 1,
                                            Inst.FreeVar [(False, 1)],
                                            Inst.StoreLocal 0,
                                            Inst.LoadLocal 1,
                                            Inst.Call []
                                          ]
                                        ),
                                      _funcConstTable =
                                        ( S.fromList
                                            [ ConstFunc
                                                ( funcObject
                                                    { _funcStackSize = 1,
                                                      _funcName = "재귀1",
                                                      _funcCode =
                                                        [Inst.LoadDeref 0, Inst.Call []]
                                                    }
                                                ),
                                              ConstFunc
                                                ( funcObject
                                                    { _funcStackSize = 1,
                                                      _funcName = "재귀2",
                                                      _funcCode =
                                                        [Inst.LoadDeref 0, Inst.Call []]
                                                    }
                                                )
                                            ]
                                        ),
                                      _funcName = "동작"
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "형용사 함수 선언 코드 생성" $
        compileStmt
          ( adjectiveDeclStmt
              "같다"
              [("값1", "와"), ("값2", "이")]
              []
              (binaryOp Equal (var "값1") (var "값2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
      it "반의어가 있는 형용사 함수 선언 코드 생성" $
        compileStmt
          ( adjectiveDeclStmt
              "같다"
              [("값1", "와"), ("값2", "이")]
              [Antonym "다르다"]
              (binaryOp Equal (var "값1") (var "값2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                ),
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "다르다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal, Inst.LogicNot]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0, Inst.Push 1, Inst.StoreGlobal 1]
                        )
      it "유의어가 있는 형용사 함수 선언 코드 생성" $
        compileStmt
          ( adjectiveDeclStmt
              "같다"
              [("값1", "와"), ("값2", "이")]
              [Synonym "똑같다"]
              (binaryOp Equal (var "값1") (var "값2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                ),
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "똑같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0, Inst.Push 1, Inst.StoreGlobal 1]
                        )
      it "유의어 및 반의어가 있는 형용사 함수 선언 코드 생성" $
        compileStmt
          ( adjectiveDeclStmt
              "같다"
              [("값1", "와"), ("값2", "이")]
              [Antonym "다르다", Synonym "똑같다"]
              (binaryOp Equal (var "값1") (var "값2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                ),
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "다르다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal, Inst.LogicNot]
                                    }
                                ),
                              ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "이"],
                                      _funcStackSize = 2,
                                      _funcName = "똑같다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Equal]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0, Inst.Push 1, Inst.StoreGlobal 1, Inst.Push 2, Inst.StoreGlobal 2]
                        )
      it "동사 함수 선언 코드 생성" $
        compileStmt
          ( verbDeclStmt
              "더하다"
              [("값1", "와"), ("값2", "을")]
              (binaryOp Add (var "값1") (var "값2"))
          )
          `shouldBuild` ( S.fromList
                            [ ConstFunc
                                ( funcObject
                                    { _funcJosa = ["와", "을"],
                                      _funcStackSize = 2,
                                      _funcName = "더하다",
                                      _funcCode =
                                        [Inst.LoadLocal 0, Inst.LoadLocal 1, Inst.Add]
                                    }
                                )
                            ],
                          [Inst.Push 0, Inst.StoreGlobal 0]
                        )
  describe "상수 선언문 코드 생성" $ do
    it "하나의 값에 대한 상수 선언문" $ do
      compileStmt (constDeclStmt "값" (litInteger 1))
        `shouldBuild` ( S.fromList [ConstInteger 1],
                        [Inst.Push 0, Inst.StoreGlobal 0]
                      )
    it "계산식 상수 선언문" $ do
      compileStmt
        (constDeclStmt "값" (binaryOp Add (litInteger 1) (litInteger 2)))
        `shouldBuild` ( S.fromList [ConstInteger 1, ConstInteger 2],
                        [Inst.Push 0, Inst.Push 1, Inst.Add, Inst.StoreGlobal 0]
                      )
  describe "구조체 선언문 코드 생성" $ do
    it "필드가 1개인 구조체 선언문" $ do
      compileStmt
        (structDeclStmt "사람" ["이름"])
        `shouldBuild` ( S.fromList
                          [ ConstFunc
                              ( funcObject
                                  { _funcJosa = ["의"],
                                    _funcName = "이름",
                                    _funcStackSize = 1,
                                    _funcCode =
                                      [ Inst.LoadLocal 0,
                                        Inst.GetField "이름"
                                      ]
                                  }
                              )
                          ],
                        [ Inst.AddStruct "사람" ["이름"],
                          Inst.Push 0,
                          Inst.StoreGlobal 0
                        ]
                      )
    it "필드가 3개인 구조체 선언문" $ do
      compileStmt
        (structDeclStmt "사람" ["이름", "키", "성별"])
        `shouldBuild` ( S.fromList
                          [ ConstFunc
                              ( funcObject
                                  { _funcJosa = ["의"],
                                    _funcName = "이름",
                                    _funcStackSize = 1,
                                    _funcCode =
                                      [ Inst.LoadLocal 0,
                                        Inst.GetField "이름"
                                      ]
                                  }
                              ),
                            ConstFunc
                              ( funcObject
                                  { _funcJosa = ["의"],
                                    _funcName = "키",
                                    _funcStackSize = 1,
                                    _funcCode =
                                      [ Inst.LoadLocal 0,
                                        Inst.GetField "키"
                                      ]
                                  }
                              ),
                            ConstFunc
                              ( funcObject
                                  { _funcJosa = ["의"],
                                    _funcName = "성별",
                                    _funcStackSize = 1,
                                    _funcCode =
                                      [ Inst.LoadLocal 0,
                                        Inst.GetField "성별"
                                      ]
                                  }
                              )
                          ],
                        [ Inst.AddStruct "사람" ["이름", "키", "성별"],
                          Inst.Push 0,
                          Inst.StoreGlobal 0,
                          Inst.Push 1,
                          Inst.StoreGlobal 1,
                          Inst.Push 2,
                          Inst.StoreGlobal 2
                        ]
                      )
