module Nuri.Codegen.Expr where

import           Control.Lens                             ( use )

import           Data.List                                ( elemIndex )

import           Nuri.Expr
import           Nuri.Literal
import           Nuri.ASTNode

import           Haneul.Builder
import           Haneul.BuilderInternal
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst

litToConst :: Literal -> Constant
litToConst LitNone        = ConstNone
litToConst (LitInteger v) = ConstInteger v
litToConst (LitReal    v) = ConstReal v
litToConst (LitChar    v) = ConstChar v
litToConst (LitBool    v) = ConstBool v

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  let value = litToConst lit
  index <- addConstant value
  tellCode [(pos, Inst.Push $ fromIntegral index)]
compileExpr (Var pos ident) = do
  names <- fmap fst . toList <$> use internalVarNames
  case elemIndex ident names of
    Just index -> tellCode [(pos, Inst.Load $ fromIntegral index)]
    Nothing    -> tellCode [(pos, Inst.LoadGlobal ident)]
compileExpr (FuncCall pos func args) = do
  compileExpr (Var pos func)
  sequence_ (compileExpr <$> args)
  tellCode [(pos, Inst.Call $ genericLength args)]
compileExpr (Seq (x :| xs)) = do
  case nonEmpty xs of
    Nothing   -> compileExpr x
    Just rest -> do
      compileExpr x
      tellCode [(getSourceLine x, Inst.Pop)]
      compileExpr (Seq rest)
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  case op of
    Add              -> tellCode [(pos, Inst.Add)]
    Subtract         -> tellCode [(pos, Inst.Subtract)]
    Multiply         -> tellCode [(pos, Inst.Multiply)]
    Divide           -> tellCode [(pos, Inst.Divide)]
    Mod              -> tellCode [(pos, Inst.Mod)]
    Equal            -> tellCode [(pos, Inst.Equal)]
    Inequal          -> tellCode [(pos, Inst.Equal), (pos, Inst.Negate)]
    LessThan         -> tellCode [(pos, Inst.LessThan)]
    GreaterThan      -> tellCode [(pos, Inst.GreaterThan)]
    LessThanEqual    -> tellCode [(pos, Inst.GreaterThan), (pos, Inst.Negate)]
    GreaterThanEqual -> tellCode [(pos, Inst.LessThan), (pos, Inst.Negate)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> pass
    Negative -> tellCode [(pos, Inst.Negate)]
compileExpr (List pos list) = do
  sequence_ $ compileExpr <$> list
  tellCode [(pos, Inst.BuildList $ genericLength list)]
