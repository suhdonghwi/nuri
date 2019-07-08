module Nuri.Spec.Eval.Util where

import Test.Hspec

import Data.Text
import Data.Map

import Nuri.Spec.Util

import Nuri.Eval.Error
import Nuri.Eval.Val

sampleTable :: SymbolTable
sampleTable = fromList
  [ ("나이" , IntegerVal 17)
  , ("십"  , FuncVal sampleFunc)
  , ("늘리기", FuncVal sampleFunc2)
  ]
 where
  sampleFunc _ = return (IntegerVal 10)
  sampleFunc2 [IntegerVal x] = return $ IntegerVal (x + 10)
  sampleFunc2 _              = undefined

funcVal :: Val
funcVal = FuncVal (\_ -> return $ IntegerVal 10)

unboundSymbol :: Text -> Error
unboundSymbol = UnboundSymbol initPos

boundSymbol :: Text -> Error
boundSymbol = BoundSymbol initPos

operateTypeError :: Text -> Text -> Error
operateTypeError = OperateTypeError initPos

notCallable :: Text -> Error
notCallable = NotCallable initPos

notInFunction :: Error
notInFunction = NotInFunction initPos

shouldEval
  :: (Eq a, Show a)
  => Either Error (a, SymbolTable)
  -> (a, SymbolTable)
  -> Expectation
shouldEval actual expected = shouldBe actual (Right expected)

shouldEvalError
  :: (Eq a, Show a) => Either Error (a, SymbolTable) -> Error -> Expectation
shouldEvalError actual expectedError = shouldBe actual (Left expectedError)
