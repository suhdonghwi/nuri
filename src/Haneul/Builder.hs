module Haneul.Builder where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Text.Megaparsec.Pos

import           Nuri.Literal

import           Haneul.Instruction

type Builder = RWS () [(Pos, Instruction)] (OSet Literal)
