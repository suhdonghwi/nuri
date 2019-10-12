module Haneul.Builder where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Control.Lens
import           Control.Lens.TH                          ( )

import           Text.Megaparsec.Pos

import           Nuri.Literal

import           Haneul.Instruction

data BuilderInternal = BuilderInternal {_constTable :: OSet Literal, _registerTable :: [Text]}
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = RWS () [(Pos, Instruction)] BuilderInternal
