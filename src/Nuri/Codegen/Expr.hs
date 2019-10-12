module Nuri.Codegen.Expr where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Nuri.Expr

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileExpr :: Expr -> Builder ()
compileExpr (Lit _ lit) = do
  modify (|> lit)
  table <- get
  let (Just index) = findIndex lit table
  tell [Inst.Push index]
compileExpr (BinaryOp _ op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  case op of
    Add      -> tell [Inst.Add]
    Subtract -> tell [Inst.Subtract]
    Multiply -> tell [Inst.Multiply]
    Divide   -> tell [Inst.Divide]
    Mod      -> tell [Inst.Mod]
compileExpr (UnaryOp _ op value) = do
  compileExpr value
  case op of
    Positive -> tell []
    Negative -> tell [Inst.Negate]
