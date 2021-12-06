module Nuri.Codegen.Stmt where

import Haneul.Builder (Builder, addGlobalVarName, tellInst)
import qualified Haneul.Instruction as Inst
import Nuri.ASTNode (ASTNode (getSourcePos))
import Nuri.Codegen.Expr (compileExpr, declToExpr)
import Nuri.Expr (Decl (Decl), DeclType (StructDecl))
import Nuri.Stmt (Stmt (..))

compileStmt :: Stmt -> Builder ()
compileStmt (DeclStmt (Decl pos name t)) = do
  let declList = declToExpr pos name t
  case t of
    StructDecl fields -> tellInst pos (Inst.AddStruct name fields)
    _ -> pass

  sequence_ (addGlobalVarName . fst <$> declList)

  let register (n, b) =
        case b of
          Just build -> do
            build
            index <- addGlobalVarName n
            tellInst pos (Inst.StoreGlobal index)
          Nothing -> pass

  sequence_ (register <$> declList)
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tellInst (getSourcePos stmt) Inst.Pop

compileStmts :: NonEmpty Stmt -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)
