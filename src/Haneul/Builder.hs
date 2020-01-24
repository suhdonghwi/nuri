module Haneul.Builder where

import           Control.Monad.RWS                        ( RWS
                                                          , tell
                                                          )
import           Control.Lens                             ( makeLenses
                                                          , modifying
                                                          , use
                                                          , uses
                                                          , element
                                                          , (.~)
                                                          , view
                                                          )
import qualified Data.Set.Ordered              as S
import           Data.Set.Ordered                         ( OSet
                                                          , (|>)
                                                          , findIndex
                                                          )
import           Data.List                                ( (!!) )

import           Haneul.Instruction
import           Haneul.Constant

type ConstTable = OSet Constant
data BuilderInternal = BuilderInternal { _internalConstTable :: ConstTable, _internalVarNames :: OSet (String, Int), _internalOffset :: Int32, _internalMarks :: [Int32] }
  deriving (Show)

instance Eq BuilderInternal where
  BuilderInternal t1 v1 _ m1 == BuilderInternal t2 v2 _ m2 =
    (t1 == t2) && (v1 == v2) && (m1 == m2)

$(makeLenses ''BuilderInternal)

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

createMark :: Builder Int32
createMark = do
  modifying internalMarks (++ [0])
  uses internalMarks (flip (-) 1 . genericLength)

setMark :: Int32 -> Builder ()
setMark markIndex = do
  offset <- use internalOffset
  modifying internalMarks (element (fromIntegral markIndex) .~ offset)

clearMarks :: BuilderInternal -> Code -> Code
clearMarks internal markedCode = fmap (unmarkInst internal) <$> markedCode

unmarkInst :: BuilderInternal -> Instruction -> Instruction
unmarkInst internal inst = case inst of
  Jmp           v -> Jmp (Value $ unmark v)
  PopJmpIfFalse v -> PopJmpIfFalse (Value $ unmark v)
  v               -> v
 where
  unmark (Mark index) =
    let marks = view internalMarks internal in marks !! fromIntegral index
  unmark (Value v) = v

tellCode :: Code -> Builder ()
tellCode code = do
  modifying internalOffset (+ genericLength code)
  tell code


