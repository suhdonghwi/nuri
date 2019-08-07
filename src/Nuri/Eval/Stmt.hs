module Nuri.Eval.Stmt where

import           Control.Monad.State
import           Control.Monad.Except

import           Control.Lens

import           Data.Map                                 ( union
                                                          , fromList
                                                          , member
                                                          , insert
                                                          , intersection
                                                          )
import qualified Data.Text                     as Text
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec.Pos

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Eval.Val
import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.ValType


scope :: Eval (Flow Val) -> Eval (Flow Val)
scope p = do
  prevTable <- view symbolTable <$> get
  result    <- p
  modify $ over symbolTable (intersection prevTable)
  return result

makeFunc :: SourcePos -> Int -> ([Val] -> Eval (Flow Val)) -> Val
makeFunc pos argsNum body =
  let func argsVal = do
        when (argsNum /= length argsVal) $ throwError $ IncorrectArgsNum
          pos
          argsNum
          (length argsVal)
        (scope . body) argsVal
  in  FuncVal func

makeFuncStmt :: SourcePos -> [Text.Text] -> Stmt -> Val
makeFuncStmt pos args body = makeFunc
  pos
  (length args)
  (\argsVal -> do
    modify $ over symbolTable ((union . fromList) (zip args argsVal))
    result <- evalStmt body True
    return result
  )

evalStmt :: Stmt -> Bool -> Eval (Flow Val)
evalStmt (Seq stmts) isInFunc =
  NE.last <$> (sequence $ (`evalStmt` isInFunc) <$> stmts)
evalStmt (ExprStmt expr) _        = Normal <$> evalExpr expr
evalStmt (Return   expr) isInFunc = if isInFunc
  then Returned <$> evalExpr expr
  else throwError $ NotInFunction (srcPos expr)
evalStmt (If pos expr thenStmt elseStmt) isInFunc = do
  result <- evalExpr expr
  let resultType = getTypeName result
  if resultType /= BoolType
    then throwError $ NotConditionType pos resultType
    else scope
      (if result == BoolVal True
        then evalStmt thenStmt isInFunc
        else case elseStmt of
          Just stmt -> evalStmt stmt isInFunc
          Nothing   -> return (Normal Undefined)
      )
evalStmt (FuncDecl pos funcName args body) _ = do
  addSymbol funcName (makeFuncStmt pos args body)
  return (Normal Undefined)
 where
  addSymbol :: Text.Text -> Val -> Eval ()
  addSymbol symbol val = do
    table <- view symbolTable <$> get
    if member symbol table
      then throwError $ BoundSymbol pos symbol
      else modify $ over symbolTable (insert symbol val)

runStmtEval :: Stmt -> EvalState -> IO (Either Error (Flow Val, EvalState))
runStmtEval stmt st = runExceptT (runStateT (unEval (evalStmt stmt False)) st)
