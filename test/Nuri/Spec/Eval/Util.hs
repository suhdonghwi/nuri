module Nuri.Spec.Eval.Util where

import           Test.Hspec

import           Data.Text

import           Nuri.Spec.Util

import           Nuri.Eval.Error
import           Nuri.Eval.Val

funcVal :: Val
funcVal = FuncVal (\_ -> return $ IntegerVal 10)

unboundSymbol :: Text -> Error
unboundSymbol = UnboundSymbol initPos

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
