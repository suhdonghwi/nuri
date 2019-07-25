module Nuri.Eval.Val where

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Map
import           Data.Text                     as Text

import           Nuri.Eval.Error

type SymbolTable = Map Text Val
type Eval = StateT SymbolTable (ExceptT Error IO)

data Val = IntegerVal Integer
         | RealVal Double
         | BoolVal Bool
         | FuncVal ([Val] -> Eval Val)
         | Undefined

instance Eq Val where
  IntegerVal v1 == IntegerVal v2 = v1 == v2
  RealVal    v1 == RealVal    v2 = v1 == v2
  BoolVal    v1 == BoolVal    v2 = v1 == v2
  FuncVal    _  == FuncVal    _  = True
  Undefined     == Undefined     = True
  _             == _             = False

instance Show Val where
  show (IntegerVal v) = "(IntegerVal " ++ show v ++ ")"
  show (RealVal    v) = "(RealVal " ++ show v ++ ")"
  show (BoolVal    v) = "(BoolVal " ++ show v ++ ")"
  show (FuncVal    _) = "(FuncVal (func))"
  show Undefined      = "(Undefined)"

getTypeName :: Val -> Text
getTypeName (IntegerVal _) = "정수"
getTypeName (RealVal    _) = "실수"
getTypeName (BoolVal    _) = "부울"
getTypeName (FuncVal    _) = "함수"
getTypeName Undefined      = "정의되지 않음"

printVal :: Val -> Text
printVal (IntegerVal v) = pack $ show v
printVal (RealVal    v) = pack $ show v
printVal (BoolVal    v) = if v then "참" else "거짓"
printVal (FuncVal    _) = "(함수)"
printVal Undefined      = "(정의되지 않음)"
