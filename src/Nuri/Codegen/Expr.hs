module Nuri.Codegen.Expr where

import           Control.Monad.RWS                        ( tell )
import           Control.Lens                             ( use )

import           Data.List                                ( elemIndex )

import           Nuri.Expr
import           Nuri.Literal

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst

litToConst :: Literal -> Constant
litToConst LitNone        = ConstNone
litToConst (LitInteger v) = ConstInteger v
litToConst (LitReal    v) = ConstReal v
litToConst (LitString  v) = ConstString v
litToConst (LitBool    v) = ConstBool v

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  let value = litToConst lit
  index <- addConstant value
  tell [(pos, Inst.Push $ fromIntegral index)]
compileExpr (Var pos ident) = do
  names <- fmap fst . toList <$> use internalVarNames
  case elemIndex ident names of
    Just index -> tell [(pos, Inst.Load $ fromIntegral index)]
    Nothing    -> tell [(pos, Inst.LoadGlobal $ Identity ident)]
compileExpr (FuncCall pos func args) = do
  compileExpr (Var pos func)
  sequence_ (compileExpr <$> args)
  tell [(pos, Inst.Call $ genericLength args)]
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
    Inequal          -> tell [(pos, Inst.Equal), (pos, Inst.Negate)]
    LessThan         -> tell [(pos, Inst.LessThan)]
    GreaterThan      -> tell [(pos, Inst.GreaterThan)]
    LessThanEqual    -> tell [(pos, Inst.GreaterThan), (pos, Inst.Negate)]
    GreaterThanEqual -> tell [(pos, Inst.LessThan), (pos, Inst.Negate)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> pass
    Negative -> tell [(pos, Inst.Negate)]
compileExpr (List pos list) = do
  sequence_ $ compileExpr <$> list
  tell [(pos, Inst.BuildList $ genericLength list)]
