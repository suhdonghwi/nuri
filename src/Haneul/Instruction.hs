module Haneul.Instruction where

import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec.Pos                      ( Pos )


data Instruction' a = Push Int32 {- 상수 테이블 인덱스 -} | Pop
                 | Store String {- 상수 이름 -} | Load String {- 상수 이름 -} | PopName
                 | Call Int32 {- 인수의 개수 -}
                 | Jmp a  {- 주소 -}
                 | PopJmpIfFalse a  {- 주소 -}
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
  deriving (Eq, Show, Ord)

newtype Mark = Mark Int32
type MarkedInstruction = Instruction' Mark
type Instruction = Instruction' Int32

type Ann t = (Pos, t)

type MarkedCode = [Ann MarkedInstruction]
type Code = [Ann Instruction]

appendInsts :: Pos -> [Instruction] -> Code -> Code
appendInsts defaultPos inst [] = (defaultPos, ) <$> inst
appendInsts _ inst code =
  code ++ (((fst . last) (NE.fromList code), ) <$> inst)
