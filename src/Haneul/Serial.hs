{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haneul.Serial where

import           Prelude                           hiding ( put
                                                          , get
                                                          , fromList
                                                          )

import           Data.Binary                              ( Binary(put, get)
                                                          , Get
                                                          )
import           Data.Set.Ordered                         ( fromList )
import           Control.Lens                             ( view )

import           Text.Megaparsec.Pos                      ( unPos
                                                          , mkPos
                                                          )

import           Haneul.Builder
import           Haneul.Constant
import           Haneul.Instruction

instance Binary BuilderInternal where
  put v = do
    put (toList $ view internalConstTable v)
    put (toList $ view internalVarNames v)
  get = liftA2 BuilderInternal (fromList <$> get) (fromList <$> get)

instance Binary Constant where
  put (ConstInteger v) = do
    put (0 :: Word8)
    put v
  put (ConstReal v) = do
    put (1 :: Word8)
    put v
  put (ConstChar v) = do
    put (2 :: Word8)
    put v
  put (ConstBool v) = do
    put (3 :: Word8)
    put v
  put (ConstFunc v) = do
    put (4 :: Word8)
    put v
  get = do
    t <- get :: Get Word8
    case t of
      0 -> ConstInteger <$> get
      1 -> ConstReal <$> get
      2 -> ConstChar <$> get
      3 -> ConstBool <$> get
      4 -> ConstFunc <$> get
      _ -> fail "invalid constant type"

instance Binary FuncObject where
  put obj = do
    put (view funcArity obj)
    put (view funcBody obj)
    put (toList $ view funcConstTable obj)
    put (toList $ view funcVarNames obj)
  get = do
    arity'      <- get
    insts'      <- get
    constTable' <- fromList <$> get
    varNames'   <- fromList <$> get
    return (FuncObject arity' insts' constTable' varNames')

instance Binary Instruction where
  put (Push v) = do
    put (0 :: Word8)
    put v
  put Pop = do
    put (1 :: Word8)
  put (Store v) = do
    put (2 :: Word8)
    put v
  put (Load v) = do
    put (3 :: Word8)
    put v
  put (Call v) = do
    put (4 :: Word8)
    put v
  put (JmpForward v) = do
    put (5 :: Word8)
    put v
  put (PopJmpIfFalse v) = do
    put (6 :: Word8)
    put v
  put Return = do
    put (7 :: Word8)
  put Add = do
    put (8 :: Word8)
  put Subtract = do
    put (9 :: Word8)
  put Multiply = do
    put (10 :: Word8)
  put Divide = do
    put (11 :: Word8)
  put Mod = do
    put (12 :: Word8)
  put Equal = do
    put (13 :: Word8)
  put LessThan = do
    put (14 :: Word8)
  put GreaterThan = do
    put (15 :: Word8)
  put Negate = do
    put (16 :: Word8)
  get = do
    inst <- get :: Get Word8
    case inst of
      0  -> Push <$> get
      1  -> return Pop
      2  -> Store <$> get
      3  -> Load <$> get
      4  -> Call <$> get
      5  -> JmpForward <$> get
      6  -> PopJmpIfFalse <$> get
      7  -> return Return
      8  -> return Add
      9  -> return Subtract
      10 -> return Multiply
      11 -> return Divide
      12 -> return Mod
      13 -> return Equal
      14 -> return LessThan
      15 -> return GreaterThan
      16 -> return Negate
      _  -> fail "invalid instruction opcode type"

instance Binary AnnInstruction  where
  put v = do
    let linePos = view lineNumber v
    put (fromIntegral (unPos linePos) :: Word32)
    put (view instruction v)
  get = do
    line <- get :: Get Word32
    AnnInst (mkPos $ fromIntegral line) <$> get

instance Binary Program where
  put p = do
    put (view programInternal p)
    put (view programCode p)
  get = liftA2 Program get get
