module Nuri.Eval.Stmt where

import           Control.Monad.State
import           Control.Monad.Except

import           Data.Map                                 ( union
                                                          , fromList
                                                          , member
                                                          , insert
                                                          , intersection
                                                          )
import           Data.List.NonEmpty                       ( toList )
import qualified Data.Text                     as Text

import           Text.Megaparsec.Pos

import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.ASTNode
import           Nuri.Eval.Val
import           Nuri.Eval.Expr
import           Nuri.Eval.Error
import           Nuri.Eval.ValType

scope :: Eval Val -> Eval Val
scope p = do
  prevTable <- get
  result    <- p
  newTable  <- get
  put $ intersection newTable prevTable
  return result

evalStmts :: [Stmt] -> Bool -> Eval Val
evalStmts (x : []) isInFunc = evalStmt x isInFunc
evalStmts (x : xs) isInFunc = evalStmt x isInFunc >> evalStmts xs isInFunc
evalStmts []       _        = return Undefined

makeFunc :: SourcePos -> Int -> ([Val] -> Eval Val) -> Val
makeFunc pos argsNum body =
  let func argsVal = do
        when (argsNum /= length argsVal) $ throwError $ IncorrectArgsNum
          pos
          argsNum
          (length argsVal)
        result <- (unEval . scope . body) argsVal
        case result of
          Undefined -> return Undefined
          v         -> return v
  in  FuncVal (Eval <$> func)

makeFuncStmt :: SourcePos -> [Text.Text] -> [Stmt] -> Val
makeFuncStmt pos args body = makeFunc
  pos
  (length args)
  (\argsVal -> do
    modify $ union (fromList $ zip args argsVal)
    result <- evalStmts body True
    return result
  )

processIfStmts
  :: SourcePos -> [(Expr, [Stmt])] -> Maybe [Stmt] -> Bool -> Eval Val
processIfStmts pos ((expr, stmts) : xs) elseStmts isInFunc = do
  exprVal <- evalExpr expr
  let valType = getTypeName exprVal
  if valType /= BoolType
    then throwError $ NotConditionType pos valType
    else if exprVal == BoolVal True
      then evalStmts stmts isInFunc
      else processIfStmts pos xs elseStmts isInFunc
processIfStmts _ [] (Just stmts) isInFunc = evalStmts stmts isInFunc
processIfStmts _ [] Nothing      _        = return Undefined

evalStmt :: Stmt -> Bool -> Eval Val
evalStmt (ExprStmt expr) _        = evalExpr expr >> return Undefined
evalStmt (Return   expr) isInFunc = if isInFunc
  then evalExpr expr >>= return
  else throwError $ NotInFunction (srcPos expr)
evalStmt (If pos condStmts elseStmts) isInFunc =
  scope $ processIfStmts pos (toList condStmts) elseStmts isInFunc
evalStmt (FuncDecl pos funcName args body) _ = do
  addSymbol funcName (makeFuncStmt pos args body)
  return Undefined
 where
  addSymbol :: Text.Text -> Val -> Eval ()
  addSymbol symbol val = do
    table <- get
    if member symbol table
      then throwError $ BoundSymbol pos symbol
      else do
        modify $ insert symbol val

runStmtEval :: Stmt -> SymbolTable -> IO (Either Error (Val, SymbolTable))
runStmtEval stmt table =
  runExceptT (runStateT (unEval (evalStmt stmt False)) table)

runStmtsEval :: [Stmt] -> SymbolTable -> IO (Either Error (Val, SymbolTable))
runStmtsEval stmt table =
  runExceptT (runStateT (unEval (evalStmts stmt False)) table)
