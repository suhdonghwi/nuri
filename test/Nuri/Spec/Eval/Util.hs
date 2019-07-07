module Nuri.Spec.Eval.Util where

import           Test.Hspec

import           Data.Text

import           Nuri.Spec.Util

import           Nuri.Eval.Error
import           Nuri.Eval.Val

unboundSymbol :: Text -> Error
unboundSymbol = UnboundSymbol initPos

operateTypeError :: Text -> Text -> Error
operateTypeError = OperateTypeError initPos

notCallable :: Text -> Error
notCallable = NotCallable initPos

shouldEval
  :: Either Error (Val, SymbolTable) -> (Val, SymbolTable) -> Expectation
shouldEval actual expected = shouldBe actual (Right expected)

shouldEvalError :: Either Error (Val, SymbolTable) -> Error -> Expectation
shouldEvalError actual expectedError = shouldBe actual (Left expectedError)
