module Nuri.Spec.Eval.Util where

import           Test.Hspec

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Text
import qualified Data.Map                      as Map

import           Nuri.Spec.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.Val
import           Nuri.Eval.Stmt
import           Nuri.Eval.ValType


testEvalWith
  :: (a -> Eval b) -> a -> EvalState -> IO (Either Error (b, EvalState))
testEvalWith f e st = runExceptT (runStateT (unEval (f e)) st)

testEval :: (a -> Eval b) -> a -> IO (Either Error (b, EvalState))
testEval f e = testEvalWith
  f
  e
  (EvalState { _symbolTable = Map.empty, _isInFunction = False })


sampleState :: EvalState
sampleState = EvalState
  { _symbolTable  =
    Map.fromList
      [ ("나이", IntegerVal 17)
      , ("십", makeFunc initPos [] (Return $ litInteger 10))
      , ( "늘리기"
        , makeFunc initPos
                   ["수"]
                   (Return $ binaryOp Plus (litInteger 10) (var "수"))
        )
      ]
  , _isInFunction = False
  }

funcVal :: Val
funcVal = makeFunc initPos [] (Return $ litInteger 10)

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
  => IO (Either Error (a, EvalState))
  -> (a, EvalState)
  -> Expectation
shouldEval actual expected = shouldReturn actual (Right expected)

shouldEvalError
  :: (Eq a, Show a) => IO (Either Error (a, EvalState)) -> Error -> Expectation
shouldEvalError actual expectedError = shouldReturn actual (Left expectedError)
