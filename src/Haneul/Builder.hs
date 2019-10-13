module Haneul.Builder where

import           Control.Monad.RWS                        ( RWS )
import           Control.Lens                             ( makeLenses )
import           Control.Lens.TH                          ( )
import           Data.Set.Ordered                         ( OSet )

import           Text.Megaparsec.Pos                      ( Pos )

import           Nuri.Codegen.Error

import           Haneul.Instruction
import           Haneul.Constant

data BuilderInternal = BuilderInternal {_constTable :: OSet Constant, _varNames :: OSet Text}
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = ExceptT Error (RWS () [(Pos, Instruction)] BuilderInternal)
