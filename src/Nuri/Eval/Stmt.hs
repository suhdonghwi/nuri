module Nuri.Eval.Stmt where

import Control.Monad.State
import Control.Monad.Except
import Data.Map

import Nuri.Stmt
import Nuri.ASTNode
import Nuri.Eval.Val
import Nuri.Eval.Expr
import Nuri.Eval.Flow
import Nuri.Eval.Error

evalStmts :: [Stmt] -> Bool -> FlowT Val Eval Val
evalStmts (x:[]) isInFunc = evalStmt x isInFunc
evalStmts (x:xs) isInFunc = evalStmt x isInFunc >> evalStmts xs isInFunc
evalStmts []     _        = return Undefined

evalStmt :: Stmt -> Bool -> FlowT Val Eval Val
evalStmt (ExprStmt expr) _            = (lift $ evalExpr expr) >>= return
evalStmt (Return   expr) isInFunction = if isInFunction
  then (lift $ evalExpr expr) >>= throw
  else lift $ throwError $ NotInFunction (srcPos expr)
evalStmt (FuncDecl _ funcName args body) _ = do
  -- TODO: 함수 인자 수 맞지 않을 시 에러
  let func argsVal = do
        prevTable <- get
        modify $ union (fromList $ zip args argsVal)
        result <- runFlowT $ evalStmts body True
        put prevTable
        case result of
          Normal _ -> return Undefined
          Thrown v -> return v
  lift . modify $ union (fromList [(funcName, FuncVal func)])
  -- TODO: 중복된 함수 이름 추가 시 에러
  return Undefined
