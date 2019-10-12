module Nuri.Codegen.Stmt where

import           Control.Monad.RWS

import           Text.Megaparsec.Pos

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
compileStmt s@(ExprStmt e) = do
  compileExpr e
  tell [(sourceLine (srcPos s), Inst.Pop)]
compileStmt s@(Return e) = do
  compileExpr e
  tell [(sourceLine (srcPos s), Inst.Return)]
compileStmt (Assign _ _ _    ) = undefined
compileStmt (If _ _ _ _      ) = undefined
compileStmt (While _ _       ) = undefined
compileStmt (FuncDecl _ _ _ _) = undefined

