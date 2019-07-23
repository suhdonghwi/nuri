module Nuri.Spec.Eval.Util where

import           Test.Hspec

import           Data.Text
import           Data.Map

import           Nuri.Spec.Util

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.Val
import           Nuri.Eval.Stmt

sampleTable :: SymbolTable
sampleTable = fromList
  [ ("나이", IntegerVal 17)
  , ("십", makeFunc initPos [] [Return $ litInteger 10])
  , ( "늘리기"
    , makeFunc initPos ["수"] [Return $ binaryOp Plus (litInteger 10) (var "수")]
    )
  ]
 where

funcVal :: Val
funcVal = makeFunc initPos [] [Return $ litInteger 10]

unboundSymbol :: Text -> Error
unboundSymbol = UnboundSymbol initPos

boundSymbol :: Text -> Error
boundSymbol = BoundSymbol initPos

operateTypeError :: [Text] -> Error
operateTypeError = OperateTypeError initPos

divideByZero :: Error
divideByZero = DivideByZero initPos

notCallable :: Text -> Error
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
