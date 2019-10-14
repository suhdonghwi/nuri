module Nuri.Codegen.Expr where

import           Control.Monad.RWS                        ( tell )
import           Control.Lens                             ( use )

import           Data.Set.Ordered                         ( findIndex )

import           Text.Megaparsec.Pos                      ( sourceLine )

import           Nuri.Expr
import           Nuri.Literal

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst

litToConst :: Literal -> Constant
litToConst (LitInteger v) = ConstInteger v
litToConst (LitReal    v) = ConstReal v
litToConst (LitChar    v) = ConstChar v
litToConst (LitBool    v) = ConstBool v

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  let value = litToConst lit
  _     <- addConstant value
  table <- use constTable
  let (Just index) = findIndex value table
  tell [(sourceLine pos, Inst.Push index)]
compileExpr (Var pos ident) = do
  names <- use varNames
  case findIndex ident names of
    Nothing -> do
      _ <- addVarName ident
      tell [(sourceLine pos, Inst.LoadBuiltin (length names))]
    Just index -> tell [(sourceLine pos, Inst.Load index)]
compileExpr (App _ _ _              ) = undefined
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  let line = sourceLine pos
  case op of
    Add              -> tell [(line, Inst.Add)]
    Subtract         -> tell [(line, Inst.Subtract)]
    Multiply         -> tell [(line, Inst.Multiply)]
    Divide           -> tell [(line, Inst.Divide)]
    Mod              -> tell [(line, Inst.Mod)]
    Equal            -> tell [(line, Inst.Equal)]
    Inequal          -> tell [(line, Inst.Inequal)]
    LessThan         -> tell [(line, Inst.LessThan)]
    GreaterThan      -> tell [(line, Inst.GreaterThan)]
    LessThanEqual    -> tell [(line, Inst.LessThanEqual)]
    GreaterThanEqual -> tell [(line, Inst.GreaterThanEqual)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> tell []
    Negative -> tell [(sourceLine pos, Inst.Negate)]
