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
normalize lhs rhs = case (lhs, rhs) of
  (RealVal    v1, RealVal v2   ) -> Just (RealVal v1, RealVal v2)
  (RealVal    v1, IntegerVal v2) -> Just (RealVal v1, RealVal (fromIntegral v2))
  (IntegerVal v1, RealVal v2   ) -> Just (RealVal (fromIntegral v1), RealVal v2)
  (IntegerVal v1, IntegerVal v2) -> Just (IntegerVal v1, IntegerVal v2)
  (BoolVal    v1, BoolVal v2   ) -> Just (BoolVal v1, BoolVal v2)
  _                              -> Nothing

operateBinary :: SourcePos -> Op -> Val -> Val -> Eval Val
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

    (Equal, v1, v2) -> return $ BoolVal (v1 == v2)
    (Inequal, v1, v2) -> return $ BoolVal (v1 /= v2)

    _ -> throwError $ OperateTypeError pos [getTypeName lhs, getTypeName rhs]


operateUnary :: SourcePos -> Op -> Val -> Eval Val
operateUnary _   Plus  v@(IntegerVal _) = return v
operateUnary _   Minus (  IntegerVal v) = return (IntegerVal (-v))
operateUnary _   Plus  v@(RealVal    _) = return v
operateUnary _   Minus (  RealVal    v) = return (RealVal (-v))
operateUnary pos _ v = throwError $ OperateTypeError pos [getTypeName v]

