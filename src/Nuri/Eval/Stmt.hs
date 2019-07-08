module Nuri.Eval.Stmt where

import Control.Monad.State
import Data.Map

import Nuri.Stmt
import Nuri.Eval.Val
import Nuri.Eval.Expr
import Nuri.Eval.Flow

evalStmts :: [Stmt] -> FlowT Val Eval Val
evalStmts (x : []) = evalStmt x
evalStmts (x : xs) = evalStmt x >> evalStmts xs
evalStmts [] = return Undefined

evalStmt :: Stmt -> FlowT Val Eval Val 
evalStmt (ExprStmt expr) = (lift $ evalExpr expr) >>= return
evalStmt (Return expr) = (lift $ evalExpr expr) >>= throw
evalStmt (FuncDecl _ funcName args body) = do
  -- TODO: 함수 인자 수 맞지 않을 시 에러
  let func argsVal = do prevTable <- get
                        modify $ union (fromList $ zip args argsVal)
                        result <- runFlowT $ evalStmts body
                        put prevTable
                        case result of
                          Normal _ -> return Undefined
                          Thrown v -> return v
  lift . modify $ union (fromList [(funcName, FuncVal func)])
  -- TODO: 중복된 함수 이름 추가 시 에러
  return Undefined
