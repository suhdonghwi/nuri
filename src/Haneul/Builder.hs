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

data BuilderInternal = BuilderInternal { _constTable :: OSet Constant, _varNames :: OSet String }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

type Builder = RWS String Code BuilderInternal

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal S.empty S.empty

addVarName :: String -> Builder Int32
addVarName ident = do
  modifying varNames (|> ident)
  names <- use varNames
  let (Just index) = findIndex ident names
  return $ fromIntegral index

addConstant :: Constant -> Builder Int32
addConstant value = do
  modifying constTable (|> value)
  names <- use constTable
  let (Just index) = findIndex value names
  return $ fromIntegral index

