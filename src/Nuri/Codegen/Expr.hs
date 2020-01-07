module Nuri.Codegen.Expr where

import           Control.Monad.RWS                        ( tell )
import           Control.Lens                             ( use )

import           Data.List                                ( elemIndex )

import           Nuri.Expr
import           Nuri.Literal

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( AnnInstruction
                                                            ( AnnInst
                                                            )
                                                          )

litToConst :: Literal -> Constant
litToConst (LitInteger v) = ConstInteger v
litToConst (LitReal    v) = ConstReal v
litToConst (LitChar    v) = ConstChar v
litToConst (LitBool    v) = ConstBool v

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  let value = litToConst lit
  index <- addConstant value
  tell [AnnInst pos (Inst.Push index)]
compileExpr (Var pos ident) = do
  names <- fmap fst . toList <$> use internalVarNames
  case elemIndex ident names of
    Just index -> tell [AnnInst pos (Inst.Load $ fromIntegral index)]
    Nothing    -> tell [AnnInst pos (Inst.LoadGlobal ident)]
compileExpr (FuncCall pos func args) = do
  compileExpr (Var pos func)
  sequence_ (compileExpr <$> args)
  tell [AnnInst pos (Inst.Call $ genericLength args)]
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  case op of
    Add         -> tell [AnnInst pos Inst.Add]
    Subtract    -> tell [AnnInst pos Inst.Subtract]
    Multiply    -> tell [AnnInst pos Inst.Multiply]
    Divide      -> tell [AnnInst pos Inst.Divide]
    Mod         -> tell [AnnInst pos Inst.Mod]
    Equal       -> tell [AnnInst pos Inst.Equal]
    Inequal     -> tell [AnnInst pos Inst.Equal, AnnInst pos Inst.Negate]
    LessThan    -> tell [AnnInst pos Inst.LessThan]
    GreaterThan -> tell [AnnInst pos Inst.GreaterThan]
    LessThanEqual ->
      tell [AnnInst pos Inst.GreaterThan, AnnInst pos Inst.Negate]
    GreaterThanEqual ->
      tell [AnnInst pos Inst.LessThan, AnnInst pos Inst.Negate]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> pass
    Negative -> tell [AnnInst pos Inst.Negate]
