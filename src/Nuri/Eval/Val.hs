module Nuri.Eval.Val where

import Control.Monad.Except
import Control.Monad.State

import Data.Map
import Data.Text

import Nuri.Eval.Error

type SymbolTable = Map Text Val
type Eval = StateT SymbolTable (Except Error)

data Val = IntegerVal Integer
         | FuncVal ([Val] -> Eval Val)
         | Undefined

instance Eq Val where
  IntegerVal v1 == IntegerVal v2 = v1 == v2
  _ == _ = True

instance Show Val where
  show (IntegerVal v) = "(IntegerVal " ++ show v ++ ")"
  show (FuncVal _) = "(FuncVal (func))"
  show Undefined = "(Undefined)"

getTypeName :: Val -> Text
getTypeName (IntegerVal _) = "정수"
getTypeName (FuncVal    _) = "함수"
getTypeName Undefined      = "정의되지 않음"
