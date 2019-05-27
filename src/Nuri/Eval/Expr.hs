module Nuri.Eval.Expr where

import           Nuri.Expr
import           Nuri.Eval.Val

evalExpr :: Expr -> Val
evalExpr (Lit _ (LitInteger v)) = IntegerVal v
