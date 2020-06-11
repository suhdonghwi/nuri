module Haneul.Builder where

import Control.Lens
  ( (.~),
    element,
    modifying,
    use,
    uses,
    view,
  )
import Control.Monad.RWS
  ( RWS,
    execRWS,
    tell,
  )
import Data.List ((!!))
import Data.Set.Ordered
  ( OSet,
    findIndex,
    (|>),
  )
import Haneul.BuilderInternal
import Haneul.Constant
import Haneul.Instruction
import Text.Megaparsec.Pos (Pos)

type Builder = RWS [OSet Text] MarkedCode BuilderInternal

addConstant :: Constant -> Builder Word32
addConstant value = do
  modifying internalConstTable (|> value)
  table <- use internalConstTable
  let (Just index) = findIndex value table
  return $ fromIntegral index

addVarName :: Word8 -> Text -> Builder Word32
addVarName depth value = do
  modifying internalLocalVars (++ [(depth, value)])
  table <- use internalLocalVars
  return $ genericLength table - 1

addGlobalVarName :: Text -> Builder Word32
addGlobalVarName value = do
  modifying internalGlobalVarNames (|> value)
  table <- use internalGlobalVarNames
  let (Just index) = findIndex value table
  return $ fromIntegral index

addFreeVar :: (Word8, Word8) -> Builder Word32
addFreeVar value = do
  modifying internalFreeVars (|> value)
  table <- use internalFreeVars
  let (Just index) = findIndex value table
  return $ fromIntegral index

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
  Jmp v -> Jmp (unmark v)
  PopJmpIfFalse v -> PopJmpIfFalse (unmark v)
  Push v -> Push v
  Pop -> Pop
  StoreGlobal v -> StoreGlobal v
  LoadLocal v -> LoadLocal v
  StoreLocal v -> StoreLocal v
  LoadDeref v -> LoadDeref v
  LoadGlobal v -> LoadGlobal v
  Call v -> Call v
  FreeVar v -> FreeVar v
  Add -> Add
  Subtract -> Subtract
  Multiply -> Multiply
  Divide -> Divide
  Mod -> Mod
  Equal -> Equal
  LessThan -> LessThan
  GreaterThan -> GreaterThan
  Negate -> Negate
  LogicNot -> LogicNot
  LogicAnd -> LogicAnd
  LogicOr -> LogicOr
  where
    unmark (Mark index) =
      let marks = view internalMarks internal in marks !! fromIntegral index

tellCode :: MarkedCode -> Builder ()
tellCode code = do
  modifying internalOffset (+ genericLength code)
  tell code

tellInst :: Pos -> MarkedInstruction -> Builder ()
tellInst pos inst = tellCode [(pos, inst)]

internalToFuncObject :: (BuilderInternal, MarkedCode) -> FuncObject
internalToFuncObject (internal, markedCode) =
  let code = clearMarks internal markedCode
   in FuncObject
        { _funcGlobalVarNames = view internalGlobalVarNames internal,
          _funcStackSize = estimateStackSize code,
          _funcMaxLocalCount = view internalMaxLocalCount internal,
          _funcConstTable = view internalConstTable internal,
          _funcCode = code,
          _funcJosa = []
        }

runBuilder :: BuilderInternal -> Builder () -> (BuilderInternal, MarkedCode)
runBuilder i b = execRWS b [] i
