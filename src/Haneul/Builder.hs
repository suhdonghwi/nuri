module Haneul.Builder where

import           Control.Monad.RWS                        ( RWS
                                                          , tell
                                                          )
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

data BuilderInternal = BuilderInternal { _internalConstTable :: OSet Constant, _internalVarNames :: OSet (String, Int), _internalOffset :: Int32, _internalMarks :: [Int32] }
  deriving (Show)

instance Eq BuilderInternal where
  BuilderInternal t1 v1 _ m1 == BuilderInternal t2 v2 _ m2 =
    (t1 == t2) && (v1 == v2) && (m1 == m2)

$(makeLenses ''BuilderInternal)

data Program = Program { _programInternal :: BuilderInternal, _programCode :: Code }
  deriving (Eq, Show)

$(makeLenses ''Program)

type Builder = RWS Int Code BuilderInternal

defaultInternal :: BuilderInternal
defaultInternal = BuilderInternal (S.singleton ConstNone) S.empty 0 []

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

tellCode :: Code -> Builder ()
tellCode code = do
  modifying internalOffset (+ genericLength code)
  tell code


