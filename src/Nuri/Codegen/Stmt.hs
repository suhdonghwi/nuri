module Nuri.Codegen.Stmt where

import           Control.Monad.RWS                        ( tell )

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileStmt :: Stmt -> Builder ()
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tell [(srcPos stmt, Inst.Pop)]
compileStmt stmt@(Return expr) = do
  compileExpr expr
  tell [(srcPos stmt, Inst.Return)]
compileStmt (Assign pos ident expr) = do
  compileExpr expr
  index <- addVarName ident
  tell [(pos, Inst.Store index)]
compileStmt If{}    = undefined
compileStmt While{} = undefined
compileStmt stmt@(FuncDecl pos funcName funcArgs body) = undefined


