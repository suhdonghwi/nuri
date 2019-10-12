module Nuri.Codegen.Stmt where

import           Nuri.Stmt

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
compileStmt
