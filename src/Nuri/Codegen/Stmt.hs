module Nuri.Codegen.Stmt where

import           Control.Lens                             ( use )

import           Text.Megaparsec.Pos                      ( Pos )

import           Nuri.Stmt
import           Nuri.Codegen.Expr

import           Haneul.Builder
import           Haneul.BuilderInternal
import           Haneul.Constant
import           Haneul.Program
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
-- compileStmt stmt@(ExprStmt expr) = do
--   compileExpr expr
--   tellCode [(getSourceLine stmt, Inst.Pop)]

-- compileStmt stmt@(Return expr) = do
--   compileExpr expr
--   tellCode [(getSourceLine stmt, Inst.Return)]

-- compileStmt (Assign pos ident expr) = do
--   compileExpr expr
--   storeVar pos ident

-- compileStmt (If pos cond thenStmts elseStmts') = do
--   compileExpr cond
--   whenFalseMark <- createMark
--   tellCode [(pos, Inst.PopJmpIfFalse $ Mark whenFalseMark)]
--   compileStmts thenStmts
--   case elseStmts' of
--     Just elseStmts -> do
--       afterElseMark <- createMark
--       tellCode [(pos, Inst.Jmp $ Mark afterElseMark)]
--       setMark whenFalseMark
--       compileStmts elseStmts
--       setMark afterElseMark
--     Nothing -> do
--       setMark whenFalseMark

-- compileStmt (While pos cond body) = do
--   beforeCondMark <- createMark
--   setMark beforeCondMark
--   compileExpr cond
--   whenFalseMark <- createMark
--   tellCode [(pos, Inst.PopJmpIfFalse $ Mark whenFalseMark)]
--   compileStmts body
--   tellCode [(pos, Inst.Jmp $ Mark beforeCondMark)]
--   setMark whenFalseMark

compileStmt (FuncDecl pos funcName argNames body) = do
  depth    <- ask
  varNames <- use internalVarNames
  let
    argCount = length argNames
    argVars  = argNames
    Program { _programConstTable = constTable, _programCode = code } =
      toProgram
        (depth + 1)
        (defaultInternal { _internalVarNames = varNames })
        (do
          sequence_ (addVarName <$> argVars)
          compileExpr body
        )
    funcObject = ConstFunc
      (FuncObject { _funcArity      = fromIntegral argCount
                  , _funcBody       = code
                  , _funcConstTable = constTable
                  }
      )
  funcObjectIndex <- addConstant funcObject
  tellCode [(pos, Inst.Push funcObjectIndex)]
  storeVar pos funcName

compileStmts :: Stmts -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)

storeVar :: Pos -> String -> Builder ()
storeVar pos ident = do
  depth <- ask
  if depth == 0
    then tellCode [(pos, Inst.StoreGlobal ident)]
    else do
      index <- addVarName ident
      tellCode [(pos, Inst.Store index)]


