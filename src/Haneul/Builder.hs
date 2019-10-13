module Haneul.Builder where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Control.Lens
import           Control.Lens.TH                          ( )

import           Text.Megaparsec.Pos

import           Nuri.Codegen.Error

import           Haneul.Instruction
import           Haneul.Constant

data BuilderInternal = BuilderInternal {_constTable :: OSet Constant, _varNames :: OSet Text}
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = ExceptT Error (RWS () [(Pos, Instruction)] BuilderInternal)
