module Haneul.Builder where

import           Control.Monad.RWS

import           Nuri.Literal

import           Haneul.Instruction

type ConstantMap = Map Int Literal

type Builder = RWS () [Instruction] ConstantMap
