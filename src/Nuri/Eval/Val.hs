module Nuri.Eval.Val where

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Map
import           Data.Text

import           Nuri.Expr
import           Nuri.Eval.Error

type SymbolTable = Map Text Val
type FuncReturn = StateT SymbolTable (Except Error) Val

data Val = IntegerVal Integer
         | FuncVal ([Val] -> FuncReturn)

instance Eq Val where
  IntegerVal v1 == IntegerVal v2 = v1 == v2
  _ == _ = True

instance Show Val where
  show (IntegerVal v) = "(IntegerVal " ++ show v ++ ")"
  show (FuncVal _) = "(FuncVal (func))"

getTypeName :: Val -> Text
getTypeName (IntegerVal _) = "정수"
getTypeName (FuncVal    _) = "함수"
