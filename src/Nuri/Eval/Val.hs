module Nuri.Eval.Val where

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Map
import           Data.Text                     as Text

import           Nuri.Eval.Error
import           Nuri.Eval.ValType

type SymbolTable = Map Text Val

newtype Eval a = Eval { unEval :: StateT SymbolTable (ExceptT Error IO) a }
  deriving (Monad, Functor, Applicative, MonadState SymbolTable, MonadError Error)

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

getTypeName :: Val -> ValType
getTypeName (IntegerVal _) = IntegerType
getTypeName (RealVal    _) = RealType
getTypeName (BoolVal    _) = BoolType
getTypeName (FuncVal    _) = FuncType
getTypeName Undefined      = UndefinedType

printVal :: Val -> Text
printVal (IntegerVal v) = pack $ show v
printVal (RealVal    v) = pack $ show v
printVal (BoolVal    v) = if v then "참" else "거짓"
printVal (FuncVal    _) = "(함수)"
printVal Undefined      = "(정의되지 않음)"
