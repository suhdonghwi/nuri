module Nuri.Eval.Val where

import           Nuri.Expr

type Func = [Expr] -> Val

data Val = IntegerVal Integer
         | FuncVal Func
