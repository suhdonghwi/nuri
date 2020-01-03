module Haneul.Instruction where

data Instruction = Push Int {- 상수 테이블 인덱스 -} | Pop
                 | Store Int {- 변수 테이블 인덱스 -} | Load Int {- 변수 테이블 인덱스 -}
                 | Call Int {- 인수의 개수 -}
                 | JmpForward Int {- 주소 오프셋 -}
                 | PopJmpIfFalse Int {- 주소 오프셋 -}
                 | Return
                 | Add | Subtract | Multiply | Divide | Mod
                 | Equal | LessThan | GreaterThan
                 | Negate
                 deriving (Eq, Show, Ord)

opcodeSize :: Int
opcodeSize = 1

operandSize :: Int
operandSize = 4

getInstSize :: Instruction -> Int
getInstSize (Push          _) = opcodeSize + operandSize
getInstSize (Store         _) = opcodeSize + operandSize
getInstSize (Load          _) = opcodeSize + operandSize
getInstSize (Call          _) = opcodeSize + operandSize
getInstSize (JmpForward    _) = opcodeSize + operandSize
getInstSize (PopJmpIfFalse _) = opcodeSize + operandSize
getInstSize (_              ) = opcodeSize
