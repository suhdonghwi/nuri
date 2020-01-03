module Haneul.Builder where

import           Control.Monad.RWS                        ( RWS )
import           Control.Lens                             ( makeLenses
                                                          , modifying
                                                          , use
                                                          )
import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( OSet
                                                          , (|>)
                                                          , findIndex
                                                          )

import           Haneul.Instruction
import           Haneul.Constant

data BuilderInternal = BuilderInternal { _constTable :: OSet Constant, _varNames :: OSet Text }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = RWS Text Code BuilderInternal

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal S.empty S.empty

addVarName :: Text -> Builder Int
addVarName ident = do
  modifying varNames (|> ident)
  names <- use varNames
  let (Just index) = findIndex ident names
  return index

addConstant :: Constant -> Builder Int
addConstant value = do
  modifying constTable (|> value)
  names <- use constTable
  let (Just index) = findIndex value names
  return index

