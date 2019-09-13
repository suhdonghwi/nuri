module Nuri.Eval.Stmt where

import           Control.Monad.Except                     ( throwError )

import           Control.Lens                             ( view
                                                          , modifying
                                                          , assign
                                                          , use
                                                          , set
                                                          )

import qualified Data.Map                      as Map
import qualified Data.Text                     as Text

import           Text.Megaparsec.Pos

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Eval.Val
import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.ValType


scope :: Interpreter (Flow Val) -> Interpreter (Flow Val)
scope p = do
  prevState <- get
  p <* put prevState

makeFuncStmt :: SourcePos -> [Text.Text] -> Stmts -> Val
makeFuncStmt pos argNames bodyStmt = makeFunc pos argNames (evalStmts bodyStmt)

makeFunc :: SourcePos -> [Text.Text] -> Interpreter (Flow Val) -> Val
makeFunc pos argNames body =
  let
    func argsVal = scope $ do
      let expectedArity = length argNames
          actualArity   = length argsVal
      when (expectedArity /= actualArity) $ throwError $ IncorrectArgsNum
        pos
        expectedArity
        actualArity

      modifying symbolTable ((Map.union . Map.fromList) (zip argNames argsVal))
      assign isInFunction True
      body
  in  FuncVal func

evalStmts :: Stmts -> Interpreter (Flow Val)
evalStmts stmts = last <$> sequence (evalStmt <$> stmts)

evalStmt :: Stmt -> Interpreter (Flow Val)
evalStmt (ExprStmt expr) = Normal <$> evalExpr expr
evalStmt (Return   expr) = do
  inFunc <- use isInFunction
  if inFunc
    then Returned <$> evalExpr expr
    else throwError $ NotInFunction (srcPos expr)
evalStmt (If pos expr thenStmt elseStmt) = do
  result <- evalExpr expr
  let resultType = getTypeName result
  when (resultType /= BoolType) $ throwError (NotConditionType pos resultType)
  scope $ if result == BoolVal True
    then evalStmts thenStmt
    else case elseStmt of
      Just stmt -> evalStmts stmt
      Nothing   -> return (Normal Undefined)

evalStmt (FuncDecl pos funcName args body) = do
  addSymbol pos funcName (makeFuncStmt pos args body)
  return (Normal Undefined)

runStmtEval
  :: Stmt -> InterpreterState -> IO (Either Error (Flow Val, InterpreterState))
runStmtEval stmt st = runExceptT (runStateT (unwrap (evalStmt stmt)) st)

runStmtsEval
  :: Stmts -> InterpreterState -> IO (Either Error (Flow Val, InterpreterState))
runStmtsEval stmts st = runExceptT (runStateT (unwrap (evalStmts stmts)) st)
