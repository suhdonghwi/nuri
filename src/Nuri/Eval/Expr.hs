module Nuri.Eval.Expr where

import           Control.Monad.Except                     ( throwError )

import           Control.Lens                             ( use )

import qualified Data.Map                      as Map

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.Expr
import           Nuri.Eval.Val
import           Nuri.Eval.Error

evalExpr :: Expr -> Interpreter Val
evalExpr (Lit _   (LitInteger v)) = return $ IntegerVal v
evalExpr (Lit _   (LitReal    v)) = return $ RealVal v
evalExpr (Lit _   (LitChar    v)) = return $ CharVal v
evalExpr (Lit _   (LitBool    v)) = return $ BoolVal v

evalExpr (Var pos ident         ) = do
  table <- use symbolTable
  case Map.lookup ident table of
    Just val -> return val
    Nothing  -> throwError $ UnboundSymbol pos ident

evalExpr (App pos func args) = do
  funcEval <- evalExpr func
  case funcEval of
    FuncVal funcVal -> do
      argsVal    <- mapM evalExpr args
      funcResult <- funcVal argsVal
      case funcResult of
        Returned resultVal -> return resultVal
        Normal   _         -> return Undefined
    val -> throwError $ NotCallable pos (getTypeName val)

evalExpr (Assign _ ident expr) = do
  val <- evalExpr expr
  addSymbol' ident val
  return val

evalExpr (BinaryOp pos op lhs rhs) = do
  lhsVal <- evalExpr lhs
  rhsVal <- evalExpr rhs
  operateBinary pos op lhsVal rhsVal

evalExpr (UnaryOp pos op expr) = do
  val <- evalExpr expr
  operateUnary pos op val

normalize :: Val -> Val -> Maybe (Val, Val)
normalize lhs rhs = case (lhs, rhs) of
  (RealVal    v1, RealVal v2   ) -> Just (RealVal v1, RealVal v2)
  (RealVal    v1, IntegerVal v2) -> Just (RealVal v1, RealVal (fromIntegral v2))
  (IntegerVal v1, RealVal v2   ) -> Just (RealVal (fromIntegral v1), RealVal v2)
  (IntegerVal v1, IntegerVal v2) -> Just (IntegerVal v1, IntegerVal v2)
  (BoolVal    v1, BoolVal v2   ) -> Just (BoolVal v1, BoolVal v2)
  (CharVal    v1, CharVal v2   ) -> Just (CharVal v1, CharVal v2)
  (FuncVal    v1, FuncVal v2   ) -> Just (FuncVal v1, FuncVal v2)
  _                              -> Nothing

operateBinary :: SourcePos -> Op -> Val -> Val -> Interpreter Val
operateBinary pos op lhs rhs = case normalize lhs rhs of
  Nothing ->
    throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]
  Just result -> calculate result

 where
  calculate (lhs', rhs') = case (op, lhs', rhs') of
    (Plus    , IntegerVal v1, IntegerVal v2) -> return $ IntegerVal (v1 + v2)
    (Plus    , RealVal v1   , RealVal v2   ) -> return $ RealVal (v1 + v2)

    (Minus   , IntegerVal v1, IntegerVal v2) -> return $ IntegerVal (v1 - v2)
    (Minus   , RealVal v1   , RealVal v2   ) -> return $ RealVal (v1 - v2)

    (Asterisk, IntegerVal v1, IntegerVal v2) -> return $ IntegerVal (v1 * v2)
    (Asterisk, RealVal v1   , RealVal v2   ) -> return $ RealVal (v1 * v2)

    (Slash   , IntegerVal v1, IntegerVal v2) -> if v2 == 0
      then throwError $ DivideByZero pos
      else return $ IntegerVal (v1 `div` v2)
    (Slash, RealVal v1, RealVal v2) -> if v2 == 0
      then throwError $ DivideByZero pos
      else return $ RealVal (v1 / v2)

    (Percent, IntegerVal v1, IntegerVal v2) -> if v2 == 0
      then throwError $ DivideByZero pos
      else return $ IntegerVal (v1 `mod` v2)

    (Equal   , v1           , v2           ) -> return $ BoolVal (v1 == v2)
    (Inequal , v1           , v2           ) -> return $ BoolVal (v1 /= v2)

    (LessThan, IntegerVal v1, IntegerVal v2) -> return $ BoolVal (v1 < v2)
    (LessThanEqual, IntegerVal v1, IntegerVal v2) ->
      return $ BoolVal (v1 <= v2)
    (GreaterThan, IntegerVal v1, IntegerVal v2) -> return $ BoolVal (v1 > v2)
    (GreaterThanEqual, IntegerVal v1, IntegerVal v2) ->
      return $ BoolVal (v1 >= v2)

    (LessThan, RealVal v1, RealVal v2) -> return $ BoolVal (v1 < v2)
    (LessThanEqual, RealVal v1, RealVal v2) -> return $ BoolVal (v1 <= v2)
    (GreaterThan, RealVal v1, RealVal v2) -> return $ BoolVal (v1 > v2)
    (GreaterThanEqual, RealVal v1, RealVal v2) -> return $ BoolVal (v1 >= v2)

    _ -> throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]


operateUnary :: SourcePos -> Op -> Val -> Interpreter Val
operateUnary _   Plus  v@(IntegerVal _) = return v
operateUnary _   Minus (  IntegerVal v) = return (IntegerVal (-v))
operateUnary _   Plus  v@(RealVal    _) = return v
operateUnary _   Minus (  RealVal    v) = return (RealVal (-v))
operateUnary pos _ v = throwError $ OperateTypeError pos [getTypeName v]

