module Nuri.Eval.Val where

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Map
import           Data.Text

import           Nuri.Expr
import           Nuri.Eval.Error

type SymbolTable = Map Text Val
type Func = Expr -> StateT SymbolTable (Except Error) Val

data Val = IntegerVal Integer
         | FuncVal Func

instance Eq Val where
  IntegerVal v1 == IntegerVal v2 = v1 == v2
  _ == _ = False

instance Show Val where
  show (IntegerVal v) = "(IntegerVal " ++ show v ++ ")"
  show (FuncVal _) = "(FuncVal (func))"
