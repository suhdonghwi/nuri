{-# LANGUAGE OverloadedLists #-}
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
  describe "대입 구문 파싱" $ do
    it "단순 정수 대입" $ do
      testParse parseAssignment "[상자] = 10"
        `shouldParse` assign "상자" (litInteger 10)
    it "(붙어있는) 단순 정수 대입" $ do
      testParse parseAssignment "[상자]=10"
        `shouldParse` assign "상자" (litInteger 10)
    it "사칙연산식 대입" $ do
      testParse parseAssignment "[상자] = 10 + 2"
        `shouldParse` assign "상자" (binaryOp Plus (litInteger 10) (litInteger 2))

  describe "반환 구문 파싱" $ do
    it "단일 정수 반환" $ do
      testParse parseReturnStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "사칙연산식 반환" $ do
      testParse parseReturnStmt "1+2 반환하다"
        `shouldParse` Return (binaryOp Plus (litInteger 1) (litInteger 2))

  describe "조건문 파싱" $ do
    it "만약 1개 (단일 조건) 조건문" $ do
      testParse parseIfStmt "만약 1 1 같다 면:\n  [값] = 1"
        `shouldParse` ifStmt (app (var "같다") [litInteger 1, litInteger 1])
                             [assign "값" (litInteger 1)]
                             Nothing
    it "만약 ~ 아니고 ~ 면 조건문" $ do
      testParse parseIfStmt "만약 참 이면:\n  [값] = 1\n아니고 참 이면: \n [값] = 2"
        `shouldParse` ifStmt
                        (litBool True)
                        [assign "값" (litInteger 1)]
                        (Just
                          [ ifStmt (litBool True)
                                   [assign "값" (litInteger 2)]
                                   Nothing
                          ]
                        )
    it "만약 ~ 아니고 ~ 면 ~ 아니면 조건문" $ do
      testParse parseIfStmt
                "만약 참 이면:\n  [값] = 1\n아니고 참 이면:\n  [값] = 2\n아니면:\n  [값] = 3"
        `shouldParse` ifStmt
                        (litBool True)
                        [assign "값" (litInteger 1)]
                        (Just
                          [ ifStmt (litBool True)
                                   [assign "값" (litInteger 2)]
                                   (Just [assign "값" (litInteger 3)])
                          ]
                        )
    it "만약 ~ 아니고 ~ 면 ~ 아니고 ~ 면 ~ 아니면 조건문" $ do
      testParse
          parseIfStmt
          "만약 참 이면:\n  [값] = 1\n아니고 참 이면:\n  [값] = 2\n아니고 참 이면:\n  [값] = 3\n아니면:\n  [값] = 4"
        `shouldParse` ifStmt
                        (litBool True)
                        [assign "값" (litInteger 1)]
                        (Just
                          [ ifStmt
                              (litBool True)
                              [assign "값" (litInteger 2)]
                              (Just
                                [ ifStmt (litBool True)
                                         [assign "값" (litInteger 3)]
                                         (Just [assign "값" (litInteger 4)])
                                ]
                              )
                          ]
                        )

  describe "함수 선언문 파싱" $ do
    it "인자가 한 개인 함수" $ do
      testParse parseFuncDecl "함수 [값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        [ ExprStmt $ app (var "더하다") [var "값", litInteger 1]
                        , Return (var "값")
                        ]

    it "인자가 여러 개인 함수" $ do
      testParse parseFuncDecl "함수 [값1] [값2] 더하다:\n  [값1] + [값2] 반환하다"
        `shouldParse` funcDecl "더하다"
                               ["값1", "값2"]
                               [Return (binaryOp Plus (var "값1") (var "값2"))]

    it "함수 이름에 띄어쓰기가 포함된 함수" $ do
      testParse parseFuncDecl
                "함수 [값1] [값2] 피보나치 구하다:\n  만약 참 이면:\n    [값2] 반환하다"
        `shouldParse` funcDecl
                        "피보나치 구하다"
                        ["값1", "값2"]
                        [ifStmt (litBool True) [Return (var "값2")] Nothing]

    it "함수 속의 함수" $ do
      testParse parseFuncDecl "함수 [값] 더하다:\n  함수 [값2] 빼다:\n    1 반환하다\n  2 반환하다"
        `shouldParse` funcDecl
                        "더하다"
                        ["값"]
                        [ funcDecl "빼다" ["값2"] [Return (litInteger 1)]
                        , Return (litInteger 2)
                        ]

    it "함수의 본문이 없으면 에러" $ do
      testParse parseFuncDecl `shouldFailOn` "함수 [값] 증가하다:"
    it "함수의 이름이 예약어면 에러" $ do
      testParse parseFuncDecl `shouldFailOn` "함수 [값] 거짓:\n  1 반환하다"
    it "예약어로 시작하는 이름의 함수" $ do
      testParse parseFuncDecl "함수 [값] 거짓하다:\n  1 반환하다"
        `shouldParse` funcDecl "거짓하다" ["값"] [Return (litInteger 1)]

  describe "~인 동안 반복문 파싱" $ do
    it "반복 참 인 동안:" $ do
      testParse parseWhileStmt "반복 참 인 동안:\n  1 보여주다"
        `shouldParse` While
                        (litBool True)
                        [ExprStmt $ app (var "보여주다") [litInteger 1]]
    it "반복 1 == 1 인 동안:" $ do
      testParse parseWhileStmt "반복 1 == 1 인 동안:\n  1 보여주다" `shouldParse` While
        (binaryOp Equal (litInteger 1) (litInteger 1))
        [ExprStmt $ app (var "보여주다") [litInteger 1]]

  describe "구문 파싱" $ do
    it "표현식 구문 파싱" $ do
      testParse parseStmt "1 + 2 줄이다" `shouldParse` ExprStmt
        (binaryOp Plus (litInteger 1) (app (var "줄이다") [litInteger 2]))
    it "반환 구문 파싱" $ do
      testParse parseStmt "1 반환하다" `shouldParse` Return (litInteger 1)
    it "조건문 파싱" $ do
      testParse
          parseStmt
          "만약 참 이면:\n  [값] = 1\n아니고 참 이면:\n  [값] = 2\n아니고 참 이면:\n  [값] = 3\n아니면:\n  [값] = 4"
        `shouldParse` ifStmt
                        (litBool True)
                        [assign "값" (litInteger 1)]
                        (Just
                          [ ifStmt
                              (litBool True)
                              [assign "값" (litInteger 2)]
                              (Just
                                [ ifStmt (litBool True)
                                         [assign "값" (litInteger 3)]
                                         (Just [assign "값" (litInteger 4)])
                                ]
                              )
                          ]
                        )
    it "인자가 한 개인 함수" $ do
      testParse parseStmt "함수 [값] 증가하다:\n  [값] 1 더하다\n  [값] 반환하다"
        `shouldParse` funcDecl
                        "증가하다"
                        ["값"]
                        [ ExprStmt $ app (var "더하다") [var "값", litInteger 1]
                        , Return (var "값")
                        ]

  describe "구문 여러 개 파싱" $ do
    it "표현식 구문 여러 개 파싱" $ do
      testParse parseStmts "1 + 2 줄이다\n3 증가하다"
        `shouldParse` [ ExprStmt
                        (binaryOp Plus
                                  (litInteger 1)
                                  (app (var "줄이다") [litInteger 2])
                        )
                      , ExprStmt (app (var "증가하다") [litInteger 3])
                      ]
    it "함수 여러 개 선언 파싱" $ do
      testParse parseStmts "함수 [값] 더하다:\n  1 반환하다\n함수 [값] 빼다:\n  2 반환하다"
        `shouldParse` [ funcDecl "더하다" ["값"] [Return (litInteger 1)]
                      , funcDecl "빼다"  ["값"] [Return (litInteger 2)]
                      ]

