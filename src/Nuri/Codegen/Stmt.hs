module Nuri.Codegen.Stmt where

import           Control.Monad.RWS                        ( tell
                                                          , execRWS
                                                          )
import           Control.Lens                             ( view
                                                          , assign
                                                          )

import           Text.Megaparsec.Pos                      ( Pos )

import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( (|<>) )

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( AnnInstruction
                                                            ( AnnInst
                                                            )
                                                          , appendInst
                                                          )

scope :: Pos -> Builder () -> Builder ()
scope pos builder = do
  st <- get
  local (+ 1) $ do
    builder
    st'          <- get
    currentDepth <- ask
    let varCount = genericLength $ filter
          (\(_, depth) -> depth == currentDepth)
          (toList $ view internalVarNames st')
    replicateM_ varCount (tell [AnnInst pos Inst.Pop])
  assign internalVarNames (view internalVarNames st)

compileStmt :: Stmt -> Builder ()
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tell [AnnInst (getSourceLine stmt) Inst.Pop]
compileStmt stmt@(Return expr) = do
  compileExpr expr
  tell [AnnInst (getSourceLine stmt) Inst.Return]
compileStmt (Assign pos ident expr) = do
  compileExpr expr
  storeVar pos ident
compileStmt (If pos cond thenStmts elseStmts) = do
  compileExpr cond
  st    <- get
  depth <- ask
  let (thenInternal, thenInsts) =
        execRWS (scope pos $ compileStmts thenStmts) depth st
  put thenInternal
  case elseStmts of
    Just elseStmts' -> do
      let (elseInternal, elseInsts) =
            execRWS (scope pos $ compileStmts elseStmts') depth thenInternal
          thenInsts' =
            appendInst pos (Inst.JmpForward $ genericLength elseInsts) thenInsts
      tell [AnnInst pos (Inst.PopJmpIfFalse $ genericLength thenInsts')]
      tell thenInsts'
      put elseInternal
      tell elseInsts
    Nothing -> do
      tell [AnnInst pos (Inst.PopJmpIfFalse $ genericLength thenInsts)]
      tell thenInsts

compileStmt While{}                               = undefined
compileStmt (FuncDecl pos funcName argNames body) = do
  depth <- ask
  st    <- get
  let
    argCount         = length argNames
    argVars          = S.fromList (fmap (, depth + 1) argNames)
    (internal, code) = execRWS
      (compileStmts body)
      depth
      (defaultInternal
        { _internalVarNames = view internalVarNames st |<> argVars
        }
      )
    funcObject = ConstFunc
      (FuncObject
        { _funcArity      = fromIntegral argCount
        , _funcBody       = code
        , _funcConstTable = view internalConstTable internal
        , _funcVarNames   = S.fromList $ fst <$> toList
                              (view internalVarNames internal)
        }
      )
  funcObjectIndex <- addConstant funcObject
  tell [AnnInst pos (Inst.Push funcObjectIndex)]
  storeVar pos funcName

compileStmts :: Stmts -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)

storeVar :: Pos -> String -> Builder ()
storeVar pos ident = do
  depth <- ask
  if depth == 0
    then tell [AnnInst pos (Inst.StoreGlobal ident)]
    else do
      index <- addVarName ident
      tell [AnnInst pos (Inst.Store index)]


