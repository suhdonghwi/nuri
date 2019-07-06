module Nuri.Eval.Stmt where

import Control.Monad.State
import Data.Map

import Nuri.Stmt
import Nuri.Eval.Val
import Nuri.Eval.Expr

evalStmts :: [Stmt] -> Eval (Maybe Val)
evalStmts (x : []) = evalStmt x
evalStmts (x : xs) = evalStmt x >> evalStmts xs
evalStmts [] = return (Just Undefined)

evalStmt :: Stmt -> Eval (Maybe Val)
evalStmt (ExprStmt expr) = evalExpr expr >> return Nothing
evalStmt (Return expr) = Just <$> evalExpr expr
evalStmt (FuncDecl _ funcName args body) = do
  let func argsVal = do prevTable <- get
                        modify $ union (fromList $ zip args argsVal)
                        result <- evalStmts body
                        put prevTable
                        case result of
                          Just val -> return val
                          Nothing -> return Undefined
  modify $ union (fromList [(funcName, FuncVal func)])
  -- TODO: 중복된 함수 이름 추가 시 에러
  return (Just Undefined)