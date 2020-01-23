module Nuri.Codegen.Stmt where

import           Control.Monad.RWS                        ( execRWS )
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
import           Haneul.Instruction                       ( appendInsts )

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
    replicateM_ varCount (tellCode [(pos, Inst.Pop)])
  assign internalVarNames (view internalVarNames st)

compileStmt :: Stmt -> Builder ()
compileStmt stmt@(ExprStmt expr) = do
  compileExpr expr
  tellCode [(getSourceLine stmt, Inst.Pop)]
compileStmt stmt@(Return expr) = do
  compileExpr expr
  tellCode [(getSourceLine stmt, Inst.Return)]
compileStmt (Assign pos ident expr) = do
  compileExpr expr
  storeVar pos ident
compileStmt (If pos cond thenStmts elseStmts) = do
  compileExpr cond
  st    <- get
  depth <- ask
  let (thenInternal, (thenInsts, _)) =
        execRWS (compileStmts thenStmts) depth st
  put thenInternal
  case elseStmts of
    Just elseStmts' -> do
      let (elseInternal, (elseInsts, _)) =
            execRWS (compileStmts elseStmts') depth thenInternal
          thenInsts' = appendInsts
            pos
            [Inst.JmpForward $ genericLength elseInsts]
            thenInsts
      tellCode [(pos, Inst.PopJmpIfFalse $ genericLength thenInsts')]
      tellCode thenInsts'
      put elseInternal
      tellCode elseInsts
    Nothing -> do
      tellCode [(pos, Inst.PopJmpIfFalse $ genericLength thenInsts)]
      tellCode thenInsts

compileStmt (While pos cond body) = do
  st    <- get
  depth <- ask
  let (condInternal, (condInsts, _)) = execRWS (compileExpr cond) depth st
  put condInternal
  tellCode condInsts
  let (bodyInternal, (bodyInsts, _)) =
        execRWS (compileStmts body) depth condInternal
      bodyInsts' = appendInsts
        pos
        [ Inst.JmpBackward
          $ genericLength condInsts
          + genericLength bodyInsts
          + 1
        ]
        bodyInsts
  tellCode [(pos, Inst.PopJmpIfFalse $ genericLength bodyInsts')]
  put bodyInternal
  tellCode bodyInsts'
  pass
compileStmt (FuncDecl pos funcName argNames body) = do
  depth <- ask
  st    <- get
  let
    argCount              = length argNames
    argVars               = S.fromList (fmap (, depth + 1) argNames)
    (internal, (code, _)) = execRWS
      (local (+ 1) $ compileStmts body)
      depth
      (defaultInternal
        { _internalVarNames = view internalVarNames st |<> argVars
        }
      )
    funcObject = ConstFunc
      (FuncObject { _funcArity = fromIntegral argCount
                  , _funcBody = appendInsts pos [Inst.Push 0, Inst.Return] code
                  , _funcConstTable = view internalConstTable internal
                  }
      )
  funcObjectIndex <- addConstant funcObject
  tellCode [(pos, Inst.Push $ fromIntegral funcObjectIndex)]
  storeVar pos funcName

compileStmts :: Stmts -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)

storeVar :: Pos -> String -> Builder ()
storeVar pos ident = do
  depth <- ask
  if depth == 0
    then tellCode [(pos, Inst.StoreGlobal $ Identity ident)]
    else do
      index <- addVarName ident
      tellCode [(pos, Inst.Store $ Identity index)]


