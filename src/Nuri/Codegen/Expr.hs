module Nuri.Codegen.Expr where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Nuri.Expr

import           Haneul.Builder
import           Haneul.Instruction

compileExpr :: Expr -> Builder ()
compileExpr (Lit _ lit) = do
  modify (|> lit)
  table <- get
  let (Just index) = findIndex lit table
  tell [Push index]
