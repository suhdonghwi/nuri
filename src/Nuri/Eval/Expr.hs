module Nuri.Eval.Expr where

import           Prelude                 hiding ( lookup )

import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Data.Map.Strict

import           Nuri.Expr
import           Nuri.Eval.Val
import           Nuri.Eval.Error

evalExpr :: Func
evalExpr (Lit _   (LitInteger v)) = return $ IntegerVal v
evalExpr (Var pos ident         ) = do
  table <- get
  case lookup ident table of
    Just val -> return val
    Nothing  -> throwError $ UnboundSymbol pos ident


