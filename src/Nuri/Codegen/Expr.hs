module Nuri.Codegen.Expr where

import           Control.Lens                             ( view
                                                          , use
                                                          , uses
                                                          , assign
                                                          , modifying
                                                          )
import           Control.Monad.RWS                        ( execRWS )
import qualified Data.List                     as L
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
  let identIndices = L.elemIndices ident (snd <$> toList varNames)
  case viaNonEmpty last identIndices of
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
  sequence_ (compileExpr . fst <$> args)
  compileExpr func
  tellCode [(pos, Inst.Call $ snd <$> reverse args)]

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

compileExpr (Seq xs) = do
  modifying internalDepth (+ 1)
  depth <- use internalDepth

  -- let collectNames []       = []
  --     collectNames (y : ys) = case y of
  --       Left decl -> do
  --         let (_, name, _) = declToExpr decl
  --         name : collectNames ys
  --       Right _ -> collectNames ys


  let process []       = pass
      process (y : ys) = do
        case y of
          Left decl -> do
            let (pos, name, expr) = declToExpr decl
            addVarName depth name
            compileExpr expr
            when ((not . null) ys) (tellCode [(pos, Inst.Store)])
          Right expr -> do
            compileExpr expr
            when (not $ null ys) (tellCode [(getSourceLine expr, Inst.Pop)])
            pass
        process ys
  process $ toList xs

  maxLocalCount <- use internalMaxLocalCount
  localCount    <- uses internalVarNames genericLength
  when (localCount > maxLocalCount) (assign internalMaxLocalCount localCount)

  modifying internalDepth (`subtract` 1)

compileExpr (Lambda pos args body) = do
  (internal, funcObject) <- lambdaToFuncObject args body
  assign internalGlobalVarNames (view internalGlobalVarNames internal)

  index <- addConstant (ConstFunc funcObject)
  tellCode [(pos, Inst.Push index)]

  let freeVarList = toList $ view internalFreeVars internal
  let processFreeVar (depth, localIndex) = if depth == 0
        then tellCode [(pos, Inst.FreeVarLocal localIndex)]
        else do
          freeIndex <- addFreeVar (depth - 1, localIndex)
          tellCode [(pos, Inst.FreeVarFree $ fromIntegral freeIndex)]
  sequence_ (processFreeVar <$> freeVarList)


lambdaToFuncObject
  :: [(String, String)] -> Expr -> Builder (BuilderInternal, FuncObject)
lambdaToFuncObject args body = do
  globalVarNames <- use internalGlobalVarNames
  varNames       <- use internalVarNames
  oldLocalStack  <- ask
  let newLocalStack    = (S.fromList . fmap snd . toList) varNames
      (internal, code) = execRWS
        (do
          sequence_ (addVarName 0 . fst <$> args)
          compileExpr body
        )
        (newLocalStack : oldLocalStack)
        defaultInternal { _internalGlobalVarNames = globalVarNames }
      constTable    = view internalConstTable internal
      maxLocalCount = view internalMaxLocalCount internal
  return
    ( internal
    , FuncObject { _funcJosa          = (snd <$> args)
                 , _funcBody          = (clearMarks internal code)
                 , _funcConstTable    = constTable
                 , _funcMaxLocalCount = maxLocalCount
                 }
    )
