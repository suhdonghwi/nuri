module Nuri.Spec.Eval.Util where

import           Test.Hspec

import           Data.Text
import qualified Data.Map                      as Map

import           Nuri.Spec.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.Val
import           Nuri.Eval.Expr
import           Nuri.Eval.Stmt
import           Nuri.Eval.ValType


testEvalWith :: Expr -> SymbolTable -> IO (Either Error (Val, SymbolTable))
testEvalWith = runEval

testEval :: Expr -> IO (Either Error (Val, SymbolTable))
testEval expr = runEval expr Map.empty

sampleTable :: SymbolTable
sampleTable = Map.fromList
  [ ("나이", IntegerVal 17)
  , ("십", makeFuncStmt initPos [] [Return $ litInteger 10])
  , ( "늘리기"
    , makeFuncStmt initPos
                   ["수"]
                   [Return $ binaryOp Plus (litInteger 10) (var "수")]
    )
  ]

funcVal :: Val
funcVal = makeFuncStmt initPos [] [Return $ litInteger 10]

unboundSymbol :: Text -> Error
unboundSymbol = UnboundSymbol initPos

boundSymbol :: Text -> Error
boundSymbol = BoundSymbol initPos

operateTypeError :: [ValType] -> Error
operateTypeError = OperateTypeError initPos

divideByZero :: Error
divideByZero = DivideByZero initPos

notCallable :: ValType -> Error
notCallable = NotCallable initPos

notInFunction :: Error
notInFunction = NotInFunction initPos

incorrectArgsNum :: Int -> Int -> Error
incorrectArgsNum = IncorrectArgsNum initPos

shouldEval
  :: (Eq a, Show a)
  => IO (Either Error (a, SymbolTable))
  -> (a, SymbolTable)
  -> Expectation
shouldEval actual expected = shouldReturn actual (Right expected)

shouldEvalError
  :: (Eq a, Show a)
  => IO (Either Error (a, SymbolTable))
  -> Error
  -> Expectation
shouldEvalError actual expectedError = shouldReturn actual (Left expectedError)
