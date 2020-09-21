module Default where

import Haneul.Builder (Builder)
import Nuri.Codegen.Stmt (compileStmt)
import Nuri.Expr (Decl (Decl), DeclType (StructDecl))
import Nuri.Stmt (Stmt (DeclStmt))
import Text.Megaparsec.Pos (initialPos)

defineDefaults :: Builder ()
defineDefaults = do
  compileStmt (DeclStmt $ Decl (initialPos "") "목록" (StructDecl ["첫번째", "나머지"]))
