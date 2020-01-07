module Nuri.Codegen.Stmt where

import           Control.Monad.RWS                        ( tell
                                                          , execRWS
                                                          )
import           Control.Lens                             ( view
                                                          , assign
                                                          )

import           Text.Megaparsec.Pos                      ( Pos )

import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( (|<>)
                                                          , (|>)
                                                          )

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( AnnInstruction
                                                            ( AnnInst
                                                            )
                                                          , prependInst
                                                          )

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
        execRWS (compileStmts thenStmts) (depth + 1) st
  assign internalConstTable (view internalConstTable thenInternal)
  st' <- get
  case elseStmts of
    Just elseStmts' -> do
      let
        (elseInternal, elseInsts) =
          execRWS (compileStmts elseStmts') (depth + 1) st'
        thenInsts' =
          prependInst pos (Inst.JmpForward $ genericLength elseInsts) thenInsts
      tell [AnnInst pos (Inst.PopJmpIfFalse $ genericLength thenInsts')]
      assign internalConstTable (view internalConstTable thenInternal)
      tell thenInsts'
      assign internalConstTable (view internalConstTable elseInternal)
      tell elseInsts
    Nothing -> do
      tell [AnnInst pos (Inst.PopJmpIfFalse $ genericLength thenInsts)]
      put thenInternal
      tell thenInsts

compileStmt While{}                               = undefined
compileStmt (FuncDecl pos funcName argNames body) = do
  depth <- ask
  st    <- get
  let
    argCount         = length argNames
    (internal, code) = execRWS
      (compileStmts body)
      depth
      (defaultInternal
        { _internalVarNames =
          (view internalVarNames st |> (funcName, depth))
            |<> S.fromList (zip argNames (replicate argCount $ depth + 1))
        }
      )
    funcObject = ConstFunc
      (FuncObject { _funcArity      = fromIntegral argCount
                  , _funcBody       = code
                  , _funcConstTable = view internalConstTable internal
                  , _funcVarNames   = view internalVarNames internal
                  }
      )
  funcObjectIndex <- addConstant funcObject
  funcNameIndex   <- addVarName funcName
  tell
    [ AnnInst pos (Inst.Push funcObjectIndex)
    , AnnInst pos (Inst.Store funcNameIndex)
    ]

compileStmts :: Stmts -> Builder ()
compileStmts s = sequence_ (compileStmt <$> s)

storeVar :: Pos -> String -> Builder ()
storeVar pos ident = do
  index <- addVarName ident
  tell [AnnInst pos (Inst.Store index)]


