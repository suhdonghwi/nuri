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

addConstant :: Constant -> Builder Word32
addConstant value = do
  modifying internalConstTable (|> value)
  table <- use internalConstTable
  let (Just index) = findIndex value table
  return $ fromIntegral index

addVarName :: String -> Builder Word32
addVarName value = do
  modifying internalVarNames (value :)
  uses internalVarNames genericLength

createMark :: Builder Word32
createMark = do
  modifying internalMarks (++ [0])
  uses internalMarks (flip (-) 1 . genericLength)

setMark :: Word32 -> Builder ()
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
  StoreGlobal v   -> StoreGlobal v
  Load        v   -> Load v
  LoadGlobal  v   -> LoadGlobal v
  Call        v   -> Call v
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


