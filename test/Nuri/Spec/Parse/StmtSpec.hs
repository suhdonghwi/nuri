module Nuri.Spec.Parse.StmtSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Parse.Stmt

import           Nuri.Spec.Util
import           Nuri.Spec.Parse.Util

spec :: Spec
spec = do
  describe "반환 구문 파싱" $ do
    it "단일 정수 반환" $ do
      testParse parseReturnStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "사칙연산식 반환" $ do
      testParse parseReturnStmt "1+2 반환하다"
        `shouldParse` Return (binaryOp Plus (litInteger 1) (litInteger 2))

  describe "조건문 파싱" $ do
    it "만약 1개 (단일 조건) 조건문" $ do
      testParse parseIfStmt "만약 1 1 같다 면:\n  [값]: 1"
        `shouldParse` ifStmt
                        (app (var "같다") [litInteger 1, litInteger 1])
                        (Seq (ExprStmt (assign "값" (litInteger 1)) :| []))
                        Nothing
    it "만약 ~ 아니고 ~ 면 조건문" $ do
      testParse parseIfStmt "만약 참 이면:\n  [값]: 1\n아니고 참 이면: \n [값]: 2"
        `shouldParse` ifStmt
                        (litBool True)
                        (Seq (ExprStmt (assign "값" (litInteger 1)) :| []))
                        (Just
                          (ifStmt
                            (litBool True)
                            (Seq (ExprStmt (assign "값" (litInteger 2)) :| []))
                            Nothing
                          )
                        )
    it "만약 ~ 아니고 ~ 면 ~ 아니면 조건문" $ do
      testParse parseIfStmt
                "만약 참 이면:\n  [값]: 1\n아니고 참 이면:\n  [값]: 2\n아니면:\n  [값]: 3"
        `shouldParse` ifStmt
                        (litBool True)
                        (Seq (ExprStmt (assign "값" (litInteger 1)) :| []))
                        (Just
                          (ifStmt
                            (litBool True)
                            (Seq (ExprStmt (assign "값" (litInteger 2)) :| []))
                            (Just
                              (Seq (ExprStmt (assign "값" (litInteger 3)) :| []))
                            )
                          )
                        )
    it "만약 ~ 아니고 ~ 면 ~ 아니고 ~ 면 ~ 아니면 조건문" $ do
      testParse
          parseIfStmt
          "만약 참 이면:\n  [값]: 1\n아니고 참 이면:\n  [값]: 2\n아니고 참 이면:\n  [값]: 3\n아니면:\n  [값]: 4"
        `shouldParse` ifStmt
                        (litBool True)
                        (Seq (ExprStmt (assign "값" (litInteger 1)) :| []))
                        (Just
                          (ifStmt
                            (litBool True)
                            (Seq (ExprStmt (assign "값" (litInteger 2)) :| []))
                            (Just
                              (ifStmt
                                (litBool True)
                                (Seq
                                  (ExprStmt (assign "값" (litInteger 3)) :| [])
                                )
                                (Just
                                  (Seq
                                    (ExprStmt (assign "값" (litInteger 4)) :| [])
                                  )
                                )
                              )
                            )
                          )
                        )

  describe "함수 선언문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse' parseFuncDecl "[값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다"
        `shouldParse` ( funcDecl
                        "증가하다"
                        ["값"]
                        (Seq
                          ((ExprStmt $ app (var "더하다") [var "값", litInteger 1])
                          :| [Return (var "값")]
                          )
                        )
                      , ["증가하다"]
                      )
    it "인자가 여러 개인 함수" $ do
      testParse' parseFuncDecl "[값1] [값2] 더하다:\n  [값1] + [값2] 반환하다"
        `shouldParse` ( funcDecl
                        "더하다"
                        ["값1", "값2"]
                        (Seq
                          (Return (binaryOp Plus (var "값1") (var "값2")) :| [])
                        )
                      , ["더하다"]
                      )
    it "함수 속의 함수" $ do
      testParse' parseFuncDecl "[값] 더하다:\n  [값2] 빼다:\n    1 반환하다\n  2 반환하다"
        `shouldParse` ( funcDecl
                        "더하다"
                        ["값"]
                        (Seq
                          (  funcDecl "빼다"
                                      ["값2"]
                                      (Seq (Return (litInteger 1) :| []))
                          :| [Return (litInteger 2)]
                          )
                        )
                      , ["더하다"]
                      )
    it "함수의 본문이 없으면 에러" $ do
      testParse parseFuncDecl `shouldFailOn` "[값] 증가하다:"
    it "함수의 이름이 예약어면 에러" $ do
      testParse parseFuncDecl `shouldFailOn` "[값] 거짓:\n  1 반환하다"
    it "예약어로 시작하는 이름의 함수" $ do
      testParse parseFuncDecl "[값] 거짓하다:\n  1 반환하다"
        `shouldParse` funcDecl "거짓하다" ["값"] (Seq (Return (litInteger 1) :| []))

  describe "구문 파싱" $ do
    it "표현식 구문 파싱" $ do
      testParse parseStmt "1 + 2 줄이다" `shouldParse` ExprStmt
        (binaryOp Plus (litInteger 1) (app (var "줄이다") [litInteger 2]))
    it "반환 구문 파싱" $ do
      testParse parseStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "조건문 파싱" $ do
      testParse
          parseStmt
          "만약 참 이면:\n  [값]: 1\n아니고 참 이면:\n  [값]: 2\n아니고 참 이면:\n  [값]: 3\n아니면:\n  [값]: 4"
        `shouldParse` ifStmt
                        (litBool True)
                        (Seq (ExprStmt (assign "값" (litInteger 1)) :| []))
                        (Just
                          (ifStmt
                            (litBool True)
                            (Seq (ExprStmt (assign "값" (litInteger 2)) :| []))
                            (Just
                              (ifStmt
                                (litBool True)
                                (Seq
                                  (ExprStmt (assign "값" (litInteger 3)) :| [])
                                )
                                (Just
                                  (Seq
                                    (ExprStmt (assign "값" (litInteger 4)) :| [])
                                  )
                                )
                              )
                            )
                          )
                        )
    it "인자가 한 개인 함수" $ do
      testParse parseStmt "[값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        (Seq
                          ((ExprStmt $ app (var "더하다") [var "값", litInteger 1])
                          :| [Return (var "값")]
                          )
                        )

  describe "구문 여러 개 파싱" $ do
    it "표현식 구문 여러 개 파싱" $ do
      testParse parseStmts "1 + 2 줄이다\n3 증가하다" `shouldParse` Seq
        (  ExprStmt
            (binaryOp Plus (litInteger 1) (app (var "줄이다") [litInteger 2]))

        :| [ExprStmt (app (var "증가하다") [litInteger 3])]
        )
    it "함수 여러 개 선언 파싱" $ do
      testParse' parseStmts "[값] 더하다:\n  1 반환하다\n[값] 빼다:\n  2 반환하다"
        `shouldParse` ( Seq
                        (  funcDecl "더하다"
                                    ["값"]
                                    (Seq (Return (litInteger 1) :| []))
                        :| [ funcDecl
                               "빼다"
                               ["값"]
                               (Seq (Return (litInteger 2) :| []))
                           ]
                        )
                      , ["빼다", "더하다"]
                      )
