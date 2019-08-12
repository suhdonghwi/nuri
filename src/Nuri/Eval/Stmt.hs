module Nuri.Eval.Stmt where

import           Control.Monad.Except                     ( throwError )

import           Control.Lens                             ( view
                                                          , over
                                                          , set
                                                          )

import qualified Data.Map                      as M
import qualified Data.Text                     as T

import           Text.Megaparsec.Pos                      ( SourcePos )

import           Nuri.Stmt                                ( Stmt(..) )
import           Nuri.ASTNode                             ( srcPos )
import qualified Nuri.Eval.Val                 as V
import           Nuri.Eval.Expr                           ( evalExpr )
import           Nuri.Eval.Error                          ( Error(..) )
import           Nuri.Eval.ValType                        ( ValType(..) )


scope :: V.Interpreter (V.Flow V.Val) -> V.Interpreter (V.Flow V.Val)
scope p = do
  prevTable <- gets (view V.symbolTable)
  result    <- p
  modify $ over V.symbolTable (M.intersection prevTable)
  return result

makeFunc :: SourcePos -> [T.Text] -> Stmt -> V.Val
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
        $ over V.symbolTable ((M.union . M.fromList) (zip argNames argsVal))
      modify $ set V.isInFunction True
      result <- evalStmt body
      modify $ set V.isInFunction False
      return result
  in  V.FuncVal func

evalStmt :: Stmt -> V.Interpreter (V.Flow V.Val)
evalStmt (Seq      stmts) = last <$> sequence (evalStmt <$> stmts)
evalStmt (ExprStmt expr ) = V.Normal <$> evalExpr expr
evalStmt (Return   expr ) = do
  inFunc <- gets (view V.isInFunction)
  if inFunc
    then V.Returned <$> evalExpr expr
    else throwError $ NotInFunction (srcPos expr)
evalStmt (If pos expr thenStmt elseStmt) = do
  result <- evalExpr expr
  let resultType = V.getTypeName result
  if resultType /= BoolType
    then throwError $ NotConditionType pos resultType
    else scope
      (if result == V.BoolVal True
        then evalStmt thenStmt
        else case elseStmt of
          Just stmt -> evalStmt stmt
          Nothing   -> return (V.Normal V.Undefined)
      )
evalStmt (FuncDecl pos funcName args body) = do
  addSymbol funcName (makeFunc pos args body)
  return (V.Normal V.Undefined)
 where
  addSymbol :: T.Text -> V.Val -> V.Interpreter ()
  addSymbol symbol val = do
    table <- gets (view V.symbolTable)
    if M.member symbol table
      then throwError $ BoundSymbol pos symbol
      else modify $ over V.symbolTable (M.insert symbol val)

runStmtEval
  :: Stmt
  -> V.InterpreterState
  -> IO (Either Error (V.Flow V.Val, V.InterpreterState))
runStmtEval stmt st = runExceptT (runStateT (V.unwrap (evalStmt stmt)) st)
