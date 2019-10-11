module Haneul.Builder where

import           Control.Monad.RWS
import           Data.Set.Ordered

import           Nuri.Literal

import           Haneul.Instruction

type Builder = RWS () [Instruction] (OSet Literal)
