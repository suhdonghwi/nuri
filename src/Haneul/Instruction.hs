module Haneul.Instruction where

import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec.Pos                      ( Pos )


data Instruction' a = Push Word32 {- 상수 테이블 인덱스 -} | Pop
                 | Load Word32 {- 스택 인덱스 -}
                 | LoadDeref Word32
                 | StoreGlobal Word32 | LoadGlobal Word32
                 | Call [String] {- 인수의 개수 -}
                 | Jmp a  {- 주소 -}
                 | PopJmpIfFalse a  {- 주소 -}
                 | FreeVarLocal Word8
                 | FreeVarFree Word8
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
  deriving (Eq, Show, Ord)

newtype Mark = Mark Word32
type MarkedInstruction = Instruction' Mark
type Instruction = Instruction' Word32

type Ann t = (Pos, t)

type MarkedCode = [Ann MarkedInstruction]
type Code = [Ann Instruction]

appendInsts :: Pos -> [Instruction] -> Code -> Code
appendInsts defaultPos inst [] = (defaultPos, ) <$> inst
appendInsts _ inst code =
  code ++ (((fst . last) (NE.fromList code), ) <$> inst)
