module Nuri.Codegen.Expr where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Text.Megaparsec.Pos

import           Nuri.Expr

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  modify (|> lit)
  table <- get
  let (Just index) = findIndex lit table
  tell [(sourceLine pos, Inst.Push index)]
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  let line = sourceLine pos
  case op of
    Add      -> tell [(line, Inst.Add)]
    Subtract -> tell [(line, Inst.Subtract)]
    Multiply -> tell [(line, Inst.Multiply)]
    Divide   -> tell [(line, Inst.Divide)]
    Mod      -> tell [(line, Inst.Mod)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> tell []
    Negative -> tell [(sourceLine pos, Inst.Negate)]
