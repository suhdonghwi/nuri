module Nuri.Codegen.Expr where

import           Control.Lens                             ( view
                                                          , use
                                                          , assign
                                                          )
import           Control.Monad.RWS                        ( execRWS )
import qualified Data.Set.Ordered              as S

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
  varNames <- use internalVarNames
  case ident `S.findIndex` varNames of
    Just index -> tellCode [(pos, Inst.Load $ fromIntegral index)]
    Nothing    -> do
      outerVars <- ask
      let result = viaNonEmpty
            head
            [ (i, j)
            | (i, scope) <- zip [0 ..] outerVars
            , (j, x    ) <- zip [0 ..] $ toList scope
            , x == ident
            ]
      case result of
        Just loc -> do
          index <- addFreeVar loc
          tellCode [(pos, Inst.LoadDeref index)]
        Nothing -> do
          index <- addGlobalVarName ident
          tellCode [(pos, Inst.LoadGlobal index)]

compileExpr (FuncCall pos func args) = do
  sequence_ (compileExpr <$> args)
  compileExpr func
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
  globalVarNames <- use internalGlobalVarNames
  varNames       <- use internalVarNames
  localStack     <- ask
  let (internal, code) = execRWS
        (do
          temp <- use internalVarNames
          sequence_ (addVarName <$> argNames)
          compileExpr body
          assign internalVarNames temp
        )
        (varNames : localStack)
        defaultInternal { _internalGlobalVarNames = globalVarNames }
      constTable = view internalConstTable internal
      arity      = genericLength argNames
      funcObject = FuncObject arity (clearMarks internal code) constTable
  assign internalGlobalVarNames (view internalGlobalVarNames internal)
  index <- addConstant (ConstFunc funcObject)
  tellCode [(pos, Inst.Push index)]

  let freeVarList = toList $ view internalFreeVars internal
  sequence_ (fmap (\x -> tellCode [(pos, Inst.PushFreeVar x)]) freeVarList)

-- compileExpr (Let pos name value expr) = undefined
compileExpr (Let pos name value expr) = do
  compileExpr (FuncCall pos (Lambda pos [name] expr) [value])
