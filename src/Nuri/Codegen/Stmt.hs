module Nuri.Codegen.Stmt where


import           Nuri.Stmt
import           Nuri.Expr
import           Nuri.Codegen.Expr
import           Nuri.ASTNode

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder Word64
compileStmt (DeclStmt decl) = do
  let (pos, name, expr) = declToExpr decl
  exprSize <- compileExpr expr

  index    <- addGlobalVarName name
  tellCode [(pos, Inst.StoreGlobal index)]

  return exprSize

compileStmt stmt@(ExprStmt expr) = do
  exprSize <- compileExpr expr
  tellCode [(getSourceLine stmt, Inst.Pop)]

  return exprSize

compileStmts :: NonEmpty Stmt -> Builder Word64
compileStmts s = sum <$> sequence (compileStmt <$> s)


