module Nuri.Codegen.Stmt where


import           Nuri.Stmt
import           Nuri.Codegen.Expr
import           Nuri.ASTNode
import           Nuri.Decl

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
compileStmt (DeclStmt decl) = do
  let (pos, name, expr) = declToExpr decl
  compileExpr expr
  tellCode [(pos, Inst.Store name)]

compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tellCode [(getSourceLine stmt, Inst.Pop)]

compileStmts :: NonEmpty Stmt -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)


