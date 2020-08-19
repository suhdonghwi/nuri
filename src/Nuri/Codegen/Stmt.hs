module Nuri.Codegen.Stmt where

import Haneul.Builder ( addGlobalVarName, tellInst, Builder )
import qualified Haneul.Instruction as Inst
import Nuri.ASTNode ( ASTNode(getSourceLine) )
import Nuri.Codegen.Expr ( compileExpr )
import Nuri.Expr ( declToExpr, Decl(Decl) )
import Nuri.Stmt ( Stmt(..) )

compileStmt :: Stmt -> Builder ()
compileStmt (DeclStmt (Decl pos kind name (Just t))) = do
  let declList = declToExpr pos kind name t
  sequence_ (addGlobalVarName . fst <$> declList)

  let register (n, b) = do
        compileExpr b
        index <- addGlobalVarName n
        tellInst pos (Inst.StoreGlobal index)

  sequence_ (register <$> declList)
compileStmt (DeclStmt (Decl _ _ name Nothing)) = addGlobalVarName name >> pass
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tellInst (getSourceLine stmt) (Inst.Pop)

compileStmts :: NonEmpty Stmt -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)
