module Haneul.Builder where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Control.Lens
import           Control.Lens.TH                          ( )

import           Text.Megaparsec.Pos

import           Nuri.Literal
import           Nuri.Codegen.Error

import           Haneul.Instruction

data BuilderInternal = BuilderInternal {_constTable :: OSet Literal, _localVarNames :: OSet Text, _globalVarNames :: OSet Text}
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = ExceptT Error (RWS () [(Pos, Instruction)] BuilderInternal)
