module Nuri.Codegen.Expr where

import           Control.Lens                             ( view )
import           Control.Monad.RWS                        ( execRWS )

import           Nuri.Expr
import           Nuri.Literal
import           Nuri.ASTNode

import           Haneul.Builder
import           Haneul.Constant
import           Haneul.BuilderInternal
import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( Mark(Mark) )

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
  tellCode [(pos, Inst.Load ident)]

compileExpr (FuncCall pos func args) = do
  compileExpr (Var pos func)
  (sequence_ . reverse) (compileExpr <$> args)
  tellCode [(pos, Inst.Call $ genericLength args)]

compileExpr (If pos condExpr thenExpr elseExpr) = do
  compileExpr condExpr
  whenFalseMark <- createMark
  tellCode [(pos, Inst.PopJmpIfFalse $ Mark whenFalseMark)]
  compileExpr thenExpr
  thenJumpMark <- createMark
  tellCode [(pos, Inst.Jmp $ Mark thenJumpMark)]
  setMark whenFalseMark
  compileExpr elseExpr
  setMark thenJumpMark

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

compileExpr (Seq (x :| xs)) = do
  case nonEmpty xs of
    Nothing   -> compileExpr x
    Just rest -> do
      compileExpr x
      tellCode [(getSourceLine x, Inst.Pop)]
      compileExpr (Seq rest)

compileExpr (Lambda pos argNames body) = do
  let (internal, code) = execRWS (compileExpr body) () defaultInternal
      constTable = view internalConstTable internal
      funcObject = FuncObject argNames (clearMarks internal code) constTable
  index <- addConstant (ConstFunc funcObject)
  tellCode [(pos, Inst.Push index)]

compileExpr (Let pos name value expr) = do
  compileExpr value
  tellCode [(pos, Inst.Store name)]
  compileExpr expr
  tellCode [(pos, Inst.PopName)]
