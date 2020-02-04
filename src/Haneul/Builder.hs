module Haneul.Builder where

import           Control.Monad.RWS                        ( RWS
                                                          , tell
                                                          )
import           Control.Lens                             ( modifying
                                                          , use
                                                          , uses
                                                          , element
                                                          , (.~)
                                                          , view
                                                          )
import           Data.Set.Ordered                         ( (|>)
                                                          , findIndex
                                                          )
import           Data.List                                ( (!!) )

import           Haneul.Instruction
import           Haneul.Constant
import           Haneul.BuilderInternal


type Builder = RWS () MarkedCode BuilderInternal

-- addVarName :: String -> Builder Int32
-- addVarName ident = do
--   depth <- ask
--   modifying internalVarNames (|> (ident, depth))
--   names <- use internalVarNames
--   let (Just index) = findIndex (ident, depth) names
--   return $ fromIntegral index

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

clearMarks :: BuilderInternal -> MarkedCode -> Code
clearMarks internal markedCode = fmap (unmarkInst internal) <$> markedCode

unmarkInst :: BuilderInternal -> MarkedInstruction -> Instruction
unmarkInst internal inst = case inst of
  Jmp           v -> Jmp (unmark v)
  PopJmpIfFalse v -> PopJmpIfFalse (unmark v)
  Push          v -> Push v
  Pop             -> Pop
  Store v         -> Store v
  Load  v         -> Load v
  PopName         -> PopName
  Call v          -> Call v
  Add             -> Add
  Subtract        -> Subtract
  Multiply        -> Multiply
  Divide          -> Divide
  Mod             -> Mod
  Equal           -> Equal
  LessThan        -> LessThan
  GreaterThan     -> GreaterThan
  Negate          -> Negate
 where
  unmark (Mark index) =
    let marks = view internalMarks internal in marks !! fromIntegral index

tellCode :: MarkedCode -> Builder ()
tellCode code = do
  modifying internalOffset (+ genericLength code)
  tell code


