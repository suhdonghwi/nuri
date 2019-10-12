module Nuri.Codegen.Stmt where

import           Control.Monad.RWS

import           Text.Megaparsec.Pos

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
compileStmt (ExprStmt e) = do
  compileExpr e
  tell [(sourceLine (srcPos e), Inst.Pop)]

