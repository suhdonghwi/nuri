module Haneul.Instruction where

import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec.Pos                      ( Pos )

data Marked a = Mark Int32 | Value a
  deriving (Eq, Show, Ord)

data Instruction = Push Int32 {- 상수 테이블 인덱스 -} | Pop
                 | Store Int32 {- 변수 테이블 인덱스 -} | StoreGlobal String {- 글로벌 변수 이름 -}
                 | Load Int32 {- 변수 테이블 인덱스 -} | LoadGlobal String {- 글로벌 변수 이름 -}
                 | Call Int32 {- 인수의 개수 -}
                 | Jmp (Marked Int32) {- 주소 -}
                 | PopJmpIfFalse (Marked Int32) {- 주소 -}
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
                 | BuildList Int32 {- 원소의 개수 -}
  deriving (Eq, Show, Ord)

type Ann t = (Pos, t)
type Code = [Ann Instruction]

appendInsts :: Pos -> [Instruction] -> Code -> Code
appendInsts defaultPos inst [] = (defaultPos, ) <$> inst
appendInsts _ inst code =
  code ++ (((fst . last) (NE.fromList code), ) <$> inst)
