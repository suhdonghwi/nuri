module Haneul.Instruction where

import qualified Data.List.NonEmpty            as NE

import           Control.Lens                             ( makeLenses
                                                          , view
                                                          )

import           Text.Megaparsec.Pos                      ( Pos )

data InstructionF f = Push (f Int32) {- 상수 테이블 인덱스 -} | Pop
                 | Store (f Int32) {- 변수 테이블 인덱스 -} | StoreGlobal (f String) {- 글로벌 변수 이름 -}
                 | Load (f Int32) {- 변수 테이블 인덱스 -} | LoadGlobal (f String) {- 글로벌 변수 이름 -}
                 | Call (f Int32) {- 인수의 개수 -}
                 | JmpForward (f Int32) {- 주소 오프셋 -} | JmpBackward (f Int32) {- 주소 오프셋 -}
                 | PopJmpIfFalse (f Int32) {- 주소 오프셋 -}
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
                 | BuildList (f Int32) {- 원소의 개수 -}

deriving instance (Eq (f Int32), Eq (f String)) => Eq (InstructionF f)
deriving instance (Show (f Int32), Show (f String)) => Show (InstructionF f)
deriving instance (Ord (f Int32), Ord (f String)) => Ord (InstructionF f)

type Instruction = InstructionF Identity

data AnnInstruction = AnnInst { _lineNumber :: Pos, _instruction :: Instruction }
  deriving (Eq, Show, Ord)

$(makeLenses ''AnnInstruction)

type Code = [AnnInstruction]

appendInsts :: Pos -> [Instruction] -> Code -> Code
appendInsts defaultPos inst [] = AnnInst defaultPos <$> inst
appendInsts _ inst code =
  code ++ (AnnInst (view lineNumber $ last (NE.fromList code)) <$> inst)
