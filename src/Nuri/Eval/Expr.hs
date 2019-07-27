module Nuri.Eval.Expr where

import           Prelude                           hiding ( lookup )

import           Control.Monad.State.Lazy
import           Control.Monad.Except

import           Data.Map

import           Text.Megaparsec.Pos

import           Nuri.Expr
import           Nuri.Eval.Val
import           Nuri.Eval.Error

evalExpr :: Expr -> Eval Val
evalExpr (Lit _   (LitInteger v)) = return $ IntegerVal v
evalExpr (Lit _   (LitReal    v)) = return $ RealVal v
evalExpr (Lit _   (LitBool    v)) = return $ BoolVal v

evalExpr (Var pos ident         ) = do
  table <- get
  case lookup ident table of
    Just val -> return val
    Nothing  -> throwError $ UnboundSymbol pos ident
evalExpr (App pos func args) = do
  funcResult <- evalExpr func
  case funcResult of
    FuncVal funcVal -> do
      argsVal <- sequence $ fmap evalExpr args
      funcVal argsVal
    val -> throwError $ NotCallable pos (getTypeName val)
evalExpr (Assign _ ident expr) = do
  val <- evalExpr expr
  modify $ insert ident val
  return val
evalExpr (BinaryOp pos op lhs rhs) = do
  lhsVal <- evalExpr lhs
  rhsVal <- evalExpr rhs
  operateBinary pos op lhsVal rhsVal
evalExpr (UnaryOp pos op expr) = do
  val <- evalExpr expr
  operateUnary pos op val

normalize :: Val -> Val -> Maybe (Val, Val)
normalize (RealVal v1) (RealVal v2) = Just (RealVal v1, RealVal v2)
normalize (RealVal v1) (IntegerVal v2) =
  Just (RealVal v1, RealVal (fromIntegral v2))
normalize (IntegerVal v1) (RealVal v2) =
  Just (RealVal (fromIntegral v1), RealVal v2)
normalize (IntegerVal v1) (IntegerVal v2) = Just (IntegerVal v1, IntegerVal v2)
normalize (BoolVal    v1) (BoolVal    v2) = Just (BoolVal v1, BoolVal v2)
normalize _               _               = Nothing

operateBinary :: SourcePos -> Op -> Val -> Val -> Eval Val
operateBinary _ Plus (IntegerVal v1) (IntegerVal v2) =
  return $ IntegerVal (v1 + v2)
operateBinary _ Plus (RealVal v1) (RealVal v2) = return $ RealVal (v1 + v2)

operateBinary _ Minus (IntegerVal v1) (IntegerVal v2) =
  return $ IntegerVal (v1 - v2)
operateBinary _ Minus (RealVal v1) (RealVal v2) = return $ RealVal (v1 - v2)

operateBinary _ Asterisk (IntegerVal v1) (IntegerVal v2) =
  return $ IntegerVal (v1 * v2)
operateBinary _ Asterisk (RealVal v1) (RealVal v2) = return $ RealVal (v1 * v2)

operateBinary pos Slash (IntegerVal v1) (IntegerVal v2) = if v2 == 0
  then throwError $ DivideByZero pos
  else return $ IntegerVal (v1 `div` v2)
operateBinary pos Slash (RealVal v1) (RealVal v2) = if v2 == 0
  then throwError $ DivideByZero pos
  else return $ RealVal (v1 / v2)

operateBinary pos Percent (IntegerVal v1) (IntegerVal v2) = if v2 == 0
  then throwError $ DivideByZero pos
  else return $ IntegerVal (v1 `mod` v2)
operateBinary pos Percent lhs rhs =
  throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]

operateBinary pos op lhs rhs = case normalize lhs rhs of
  Just (lhs', rhs') -> operateBinary pos op lhs' rhs' `catchError` \e ->
    case e of
      OperateTypeError _ _ ->
        throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]
      other -> throwError other
  Nothing ->
    throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]

operateUnary :: SourcePos -> Op -> Val -> Eval Val
operateUnary _ Plus v@(IntegerVal _) = return v
operateUnary _ Minus (IntegerVal v) = return (IntegerVal (-v))
operateUnary pos _ v = throwError $ OperateTypeError pos [getTypeName v]

runEval :: Expr -> SymbolTable -> IO (Either Error (Val, SymbolTable))
runEval expr table = runExceptT (runStateT (evalExpr expr) table)
