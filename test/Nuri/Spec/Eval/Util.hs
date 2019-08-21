module Nuri.Spec.Eval.Util where

import           Test.Hspec

import qualified Data.Map                      as M

import           Nuri.Spec.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.Val
import           Nuri.Eval.Stmt
import           Nuri.Eval.ValType


testEvalWith
  :: (a -> Interpreter b)
  -> a
  -> InterpreterState
  -> IO (Either Error (b, InterpreterState))
testEvalWith f e st = runExceptT (runStateT (unwrap (f e)) st)

testEval :: (a -> Interpreter b) -> a -> IO (Either Error (b, InterpreterState))
testEval f e = testEvalWith
  f
  e
  (InterpreterState { _symbolTable = M.empty, _isInFunction = False })


sampleState :: InterpreterState
sampleState = InterpreterState
  { _symbolTable  =
    M.fromList
      [ ("나이", IntegerVal 17)
      , ("십", makeFuncStmt initPos [] (Return $ litInteger 10))
      , ( "늘리기"
        , makeFuncStmt initPos
                       ["수"]
                       (Return $ binaryOp Plus (litInteger 10) (var "수"))
        )
      ]
  , _isInFunction = False
  }

funcVal :: Val
funcVal = makeFuncStmt initPos [] (Return $ litInteger 10)

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
  => IO (Either Error (a, InterpreterState))
  -> (a, InterpreterState)
  -> Expectation
shouldEval actual expected = shouldReturn actual (Right expected)

shouldEvalError
  :: (Eq a, Show a)
  => IO (Either Error (a, InterpreterState))
  -> Error
  -> Expectation
shouldEvalError actual expectedError = shouldReturn actual (Left expectedError)
