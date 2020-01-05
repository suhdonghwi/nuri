module Nuri.Codegen.Stmt where

import           Control.Monad.RWS                        ( tell
                                                          , execRWS
                                                          )
import           Control.Lens                             ( view )

import           Text.Megaparsec.Pos                      ( Pos )

import qualified Data.Set.Ordered              as S

import           Nuri.Stmt
import           Nuri.ASTNode
import           Nuri.Codegen.Expr

import           Haneul.Builder
import           Haneul.Constant
import qualified Haneul.Instruction            as Inst
import           Haneul.Instruction                       ( AnnInstruction
                                                            ( AnnInst
                                                            )
                                                          , getCodeSize
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
compileStmt (If pos cond thenStmt elseStmt') = do
  compileExpr cond
  st       <- get
  fileName <- ask
  let (thenInternal, thenInsts) =
        execRWS (sequence_ (compileStmt <$> thenStmt)) fileName st
  case elseStmt' of
    Just elseStmt -> do
      let (elseInternal, elseInsts) =
            execRWS (sequence_ (compileStmt <$> elseStmt)) fileName thenInternal
          thenInsts' =
            prependInst pos (Inst.JmpForward (getCodeSize elseInsts)) thenInsts
      tell [AnnInst pos (Inst.PopJmpIfFalse (getCodeSize thenInsts'))]
      put thenInternal
      tell thenInsts'
      put elseInternal
      tell elseInsts
    Nothing -> do
      tell [AnnInst pos (Inst.PopJmpIfFalse (getCodeSize thenInsts))]
      put thenInternal
      tell thenInsts

compileStmt While{}                               = undefined
compileStmt (FuncDecl pos funcName argNames body) = do
  fileName <- ask
  let (internal, code) = execRWS
        (sequence_ (compileStmt <$> body))
        fileName
        (defaultInternal { _internalVarNames = S.fromList argNames })
      funcObject = ConstFunc
        (FuncObject { _funcArity      = fromIntegral (length argNames)
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

storeVar :: Pos -> String -> Builder ()
storeVar pos ident = do
  index <- addVarName ident
  tell [AnnInst pos (Inst.Store index)]


