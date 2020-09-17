module Haneul.Instruction where

data Instruction' a
  = Push Word32
  | Pop
  | LoadLocal Word32
  | StoreLocal Word32
  | LoadDeref Word32
  | StoreGlobal Word32
  | LoadGlobal Word32
  | Call [Text]
  | AddStruct Text [Text]
  | MakeStruct Text [Text]
  | GetField Text
  | Jmp a
  | PopJmpIfFalse a
  | FreeVar [(Bool, Word8)]
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Equal
  | LessThan
  | GreaterThan
  | Negate
  | LogicNot
  | LogicAnd
  | LogicOr
  deriving (Eq, Show, Ord)

newtype Mark = Mark Word32

type MarkedInstruction = Instruction' Mark
type Instruction = Instruction' Word32
type MarkedCode = [MarkedInstruction]
type Code = [Instruction]

estimateStackSize :: Code -> Word64
estimateStackSize input = sizeLoop 0 0 0 input
  where
    sizeLoop :: Word32 -> Word64 -> Word64 -> Code -> Word64
    sizeLoop pos currentSize maxSize code = case code !!? fromIntegral pos of
      Nothing -> maxSize
      Just inst ->
        let newSize = currentSize + sizeDifference inst
            newMaxSize = if newSize > maxSize then newSize else maxSize
         in case inst of
              PopJmpIfFalse index ->
                max
                  (sizeLoop (pos + 1) newSize newMaxSize code)
                  (sizeLoop index newSize newMaxSize code)
              Jmp index -> sizeLoop index newSize newMaxSize code
              _ -> sizeLoop (pos + 1) newSize newMaxSize code
    sizeDifference inst = case inst of
      Push _ -> 1
      LoadLocal _ -> 1
      LoadDeref _ -> 1
      LoadGlobal _ -> 1
      Call args -> - genericLength args
      AddStruct _ _ -> 0
      MakeStruct _ fields -> (- genericLength fields) + 1
      GetField _ -> 0
      Jmp _ -> 0
      PopJmpIfFalse _ -> -1
      FreeVar _ -> 0
      Negate -> 0
      LogicNot -> 0
      _ -> -1 -- 이항 연산 인스트럭션들
