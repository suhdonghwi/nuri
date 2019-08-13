module Nuri.Eval.Stmt where

import           Control.Monad.Except                     ( throwError )

import           Control.Lens                             ( view
                                                          , over
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
  prevTable <- gets (view symbolTable)
  result    <- p
  modify $ over symbolTable (Map.intersection prevTable)
  return result

makeFunc :: SourcePos -> [Text.Text] -> Stmt -> Val
makeFunc pos argNames body =
  let
    func argsVal = scope $ do
      let expectedArity = length argNames
          actualArity   = length argsVal
      when (expectedArity /= actualArity) $ throwError $ IncorrectArgsNum
        pos
        expectedArity
        actualArity

      modify
        $ over symbolTable ((Map.union . Map.fromList) (zip argNames argsVal))
      modify $ set isInFunction True
      result <- evalStmt body
      modify $ set isInFunction False
      return result
  in  FuncVal func

evalStmt :: Stmt -> Interpreter (Flow Val)
evalStmt (Seq      stmts) = last <$> sequence (evalStmt <$> stmts)
evalStmt (ExprStmt expr ) = Normal <$> evalExpr expr
evalStmt (Return   expr ) = do
  inFunc <- gets (view isInFunction)
  if inFunc
    then Returned <$> evalExpr expr
    else throwError $ NotInFunction (srcPos expr)
evalStmt (If pos expr thenStmt elseStmt) = do
  result <- evalExpr expr
  let resultType = getTypeName result
  if resultType /= BoolType
    then throwError $ NotConditionType pos resultType
    else scope
      (if result == BoolVal True
        then evalStmt thenStmt
        else case elseStmt of
          Just stmt -> evalStmt stmt
          Nothing   -> return (Normal Undefined)
      )
evalStmt (FuncDecl pos funcName args body) = do
  addSymbol funcName (makeFunc pos args body)
  return (Normal Undefined)
 where
  addSymbol :: Text.Text -> Val -> Interpreter ()
  addSymbol symbol val = do
    table <- gets (view symbolTable)
    if Map.member symbol table
      then throwError $ BoundSymbol pos symbol
      else modify $ over symbolTable (Map.insert symbol val)

runStmtEval
  :: Stmt -> InterpreterState -> IO (Either Error (Flow Val, InterpreterState))
runStmtEval stmt st = runExceptT (runStateT (unwrap (evalStmt stmt)) st)
