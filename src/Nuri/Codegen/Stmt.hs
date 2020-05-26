module Nuri.Codegen.Stmt where

import Haneul.Builder
import qualified Haneul.Instruction as Inst
import Nuri.ASTNode
import Nuri.Codegen.Expr
import Nuri.Expr
import Nuri.Stmt

compileStmt :: Stmt -> Builder ()
compileStmt (DeclStmt decl) = do
  let (pos, name, expr) = declToExpr decl
  compileExpr expr

  index <- addGlobalVarName name
  tellInst pos (Inst.StoreGlobal index)
compileStmt stmt@(ExprStmt expr) = do
  exprSize <- compileExpr expr
  tellInst (getSourceLine stmt) (Inst.Pop)

  return exprSize

compileStmts :: NonEmpty Stmt -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)
