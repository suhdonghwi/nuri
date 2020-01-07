module Haneul.Instruction where

import qualified Data.List.NonEmpty            as NE

import           Control.Lens                             ( makeLenses
                                                          , view
                                                          )

import           Text.Megaparsec.Pos                      ( Pos )

data Instruction = Push Int32 {- 상수 테이블 인덱스 -} | Pop
                 | Store Int32 {- 변수 테이블 인덱스 -} | Load Int32 {- 변수 테이블 인덱스 -} | LoadGlobal String {- 글로벌 변수 이름 -}
                 | Call Int32 {- 인수의 개수 -}
                 | JmpForward Int32 {- 주소 오프셋 -}
                 | PopJmpIfFalse Int32 {- 주소 오프셋 -}
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
                 deriving (Eq, Show, Ord)

data AnnInstruction = AnnInst { _lineNumber :: Pos, _instruction :: Instruction }
  deriving (Eq, Show, Ord)

$(makeLenses ''AnnInstruction)

type Code = [AnnInstruction]

appendInst :: Pos -> Instruction -> Code -> Code
appendInst defaultPos inst [] = [AnnInst defaultPos inst]
appendInst _ inst code =
  code ++ [AnnInst (view lineNumber $ last (NE.fromList code)) inst]
