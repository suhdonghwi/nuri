module Nuri.Codegen.Stmt where

import Haneul.Builder (Builder, addGlobalVarName, tellInst)
import qualified Haneul.Instruction as Inst
import Nuri.ASTNode (ASTNode (getSourcePos))
import Nuri.Codegen.Expr (compileExpr, declToExpr)
import Nuri.Expr (Decl (Decl))
import Nuri.Stmt (Stmt (..))

compileStmt :: Stmt -> Builder ()
compileStmt (DeclStmt (Decl pos name (Just t))) = do
  let declList = declToExpr pos name t
  sequence_ (addGlobalVarName . fst <$> declList)

  let register (n, build) = do
        build
        index <- addGlobalVarName n
        tellInst pos (Inst.StoreGlobal index)

  sequence_ (register <$> declList)
compileStmt (DeclStmt (Decl _ name Nothing)) = addGlobalVarName name >> pass
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tellInst (getSourcePos stmt) (Inst.Pop)

compileStmts :: NonEmpty Stmt -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)
