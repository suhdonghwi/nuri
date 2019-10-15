module Nuri.Codegen.Expr where

import           Control.Monad.RWS                        ( tell )
import           Control.Lens                             ( use )

import           Data.Set.Ordered                         ( findIndex )

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
  tell [(pos, Inst.Push index)]
compileExpr (Var pos ident) = do
  names <- use varNames
  case findIndex ident names of
    Nothing -> do
      _ <- addVarName ident
      tell [(pos, Inst.LoadBuiltin (length names))]
    Just index -> tell [(pos, Inst.Load index)]
compileExpr (App _ _ _              ) = undefined
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  case op of
    Add              -> tell [(pos, Inst.Add)]
    Subtract         -> tell [(pos, Inst.Subtract)]
    Multiply         -> tell [(pos, Inst.Multiply)]
    Divide           -> tell [(pos, Inst.Divide)]
    Mod              -> tell [(pos, Inst.Mod)]
    Equal            -> tell [(pos, Inst.Equal)]
    Inequal          -> tell [(pos, Inst.Inequal)]
    LessThan         -> tell [(pos, Inst.LessThan)]
    GreaterThan      -> tell [(pos, Inst.GreaterThan)]
    LessThanEqual    -> tell [(pos, Inst.LessThanEqual)]
    GreaterThanEqual -> tell [(pos, Inst.GreaterThanEqual)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> tell []
    Negative -> tell [(pos, Inst.Negate)]
