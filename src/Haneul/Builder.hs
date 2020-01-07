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

data BuilderInternal = BuilderInternal { _internalConstTable :: OSet Constant, _internalVarNames :: OSet (String, Int) }
  deriving (Eq, Show)

$(makeLenses ''BuilderInternal)

data Program = Program { _programInternal :: BuilderInternal, _programCode :: Code }
  deriving (Eq, Show)

$(makeLenses ''Program)

type Builder = RWS Int Code BuilderInternal

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal S.empty S.empty

addVarName :: String -> Builder Int32
addVarName ident = do
  depth <- ask
  modifying internalVarNames (|> (ident, depth))
  names <- use internalVarNames
  let (Just index) = findIndex (ident, depth) names
  return $ fromIntegral index

addConstant :: Constant -> Builder Int32
addConstant value = do
  modifying internalConstTable (|> value)
  names <- use internalConstTable
  let (Just index) = findIndex value names
  return $ fromIntegral index

