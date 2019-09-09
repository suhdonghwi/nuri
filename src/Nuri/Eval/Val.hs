module Nuri.Eval.Val where

import           Control.Monad.Except                     ( MonadError )

import           Control.Lens                             ( makeLenses )

import qualified Data.Map                      as Map
import qualified Text.Show

import           Nuri.Eval.Error
import           Nuri.Eval.ValType

newtype Interpreter a = Interpreter { unwrap :: StateT InterpreterState (ExceptT Error IO) a }
  deriving (Monad, Functor, Applicative, MonadState InterpreterState, MonadError Error, MonadIO)

data Flow a = Returned a | Normal a
  deriving (Eq, Show)

data Val = IntegerVal Integer
         | RealVal Double
         | CharVal Char
         | BoolVal Bool
         | FuncVal ([Val] -> Interpreter (Flow Val))
         | Undefined

instance Show Val where
  show (IntegerVal v) = "(IntegerVal " ++ show v ++ ")"
  show (RealVal    v) = "(RealVal " ++ show v ++ ")"
  show (CharVal    v) = "(CharVal " ++ show v ++ ")"
  show (BoolVal    v) = "(BoolVal " ++ show v ++ ")"
  show (FuncVal    _) = "(FuncVal (func))"
  show Undefined      = "(Undefined)"

instance Eq Val where
  IntegerVal v1 == IntegerVal v2 = v1 == v2
  RealVal    v1 == RealVal    v2 = v1 == v2
  CharVal    v1 == CharVal    v2 = v1 == v2
  BoolVal    v1 == BoolVal    v2 = v1 == v2
  FuncVal    _  == FuncVal    _  = True
  Undefined     == Undefined     = True
  _             == _             = False

getTypeName :: Val -> ValType
getTypeName (IntegerVal _) = IntegerType
getTypeName (RealVal    _) = RealType
getTypeName (CharVal    _) = CharType
getTypeName (BoolVal    _) = BoolType
getTypeName (FuncVal    _) = FuncType
getTypeName Undefined      = UndefinedType

printVal :: Val -> Text
printVal (IntegerVal v) = show v
printVal (RealVal    v) = show v
printVal (CharVal    v) = one v
printVal (BoolVal    v) = if v then "참" else "거짓"
printVal (FuncVal    _) = "(함수)"
printVal Undefined      = "(정의되지 않음)"

type SymbolTable = Map.Map Text Val
data InterpreterState = InterpreterState { _symbolTable :: SymbolTable, _isInFunction :: Bool }
  deriving (Eq, Show)

$(makeLenses ''InterpreterState)

initState :: InterpreterState
initState =
  InterpreterState { _symbolTable = Map.empty, _isInFunction = False }
