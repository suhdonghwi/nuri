module Nuri.Eval.SymbolTable where

import           Data.Map
import           Data.Text

import           Nuri.Eval.Val

type SymbolTable = Map Text Val
