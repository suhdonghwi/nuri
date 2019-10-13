module Nuri.Codegen.Expr where

import           Control.Monad.RWS                        ( tell )
import           Control.Lens                             ( modifying
                                                          , use
                                                          )
import           Control.Monad.Except                     ( throwError )

import           Data.Set.Ordered                         ( (|>)
                                                          , findIndex
                                                          )
import           Data.List                                ( elemIndices )

import           Text.Megaparsec.Pos                      ( sourceLine )

import           Nuri.Expr
import           Nuri.Codegen.Error

import           Haneul.Builder
import qualified Haneul.Instruction            as Inst

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  modifying constTable (|> lit)
  table <- use constTable
  let (Just index) = findIndex lit table
  tell [(sourceLine pos, Inst.Push index)]
compileExpr (Var pos ident) = do
  register <- use registerTable
  case nonEmpty $ elemIndices ident register of
    Nothing      -> throwError UnboundVariable
    Just indices -> tell [(sourceLine pos, Inst.Load (last indices))]
compileExpr (App _ _ _              ) = undefined
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  let line = sourceLine pos
  case op of
    Add              -> tell [(line, Inst.Add)]
    Subtract         -> tell [(line, Inst.Subtract)]
    Multiply         -> tell [(line, Inst.Multiply)]
    Divide           -> tell [(line, Inst.Divide)]
    Mod              -> tell [(line, Inst.Mod)]
    Equal            -> tell [(line, Inst.Equal)]
    Inequal          -> tell [(line, Inst.Inequal)]
    LessThan         -> tell [(line, Inst.LessThan)]
    GreaterThan      -> tell [(line, Inst.GreaterThan)]
    LessThanEqual    -> tell [(line, Inst.LessThanEqual)]
    GreaterThanEqual -> tell [(line, Inst.GreaterThanEqual)]
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> tell []
    Negative -> tell [(sourceLine pos, Inst.Negate)]
