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
import qualified Data.Set.Ordered              as S
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
  get = liftA2 BuilderInternal (fromList <$> get) (pure S.empty)

instance Binary Constant where
  put ConstNone = do
    put (0 :: Word8)
  put (ConstInteger v) = do
    put (1 :: Word8)
    put v
  put (ConstReal v) = do
    put (2 :: Word8)
    let (base, e) = decodeFloat v
    put (fromIntegral base :: Int64)
    put e
  put (ConstString v) = do
    put (3 :: Word8)
    put v
  put (ConstBool v) = do
    put (4 :: Word8)
    put v
  put (ConstFunc v) = do
    put (5 :: Word8)
    put v
  get = do
    t <- get :: Get Word8
    case t of
      0 -> return ConstNone
      1 -> ConstInteger <$> get
      2 -> do
        base <- get :: Get Int64
        e    <- get :: Get Int
        return $ ConstReal (encodeFloat (fromIntegral base) e)
      3 -> ConstString <$> get
      4 -> ConstBool <$> get
      5 -> ConstFunc <$> get
      _ -> fail "invalid constant type"

instance Binary FuncObject where
  put obj = do
    put (view funcArity obj)
    put (toList $ view funcConstTable obj)
    put (view funcBody obj)
  get = do
    arity'      <- get
    constTable' <- fromList <$> get
    insts'      <- get
    return (FuncObject arity' insts' constTable')

instance (Binary (f Int32), Binary (f String)) => Binary (InstructionF f) where
  put (Push v) = do
    put (0 :: Word8)
    put v
  put Pop = do
    put (1 :: Word8)
  put (Store v) = do
    put (2 :: Word8)
    put v
  put (StoreGlobal v) = do
    put (3 :: Word8)
    put v
  put (Load v) = do
    put (4 :: Word8)
    put v
  put (LoadGlobal v) = do
    put (5 :: Word8)
    put v
  put (Call v) = do
    put (6 :: Word8)
    put v
  put (JmpForward v) = do
    put (7 :: Word8)
    put v
  put (JmpBackward v) = do
    put (8 :: Word8)
    put v
  put (PopJmpIfFalse v) = do
    put (9 :: Word8)
    put v
  put Return = do
    put (10 :: Word8)
  put Add = do
    put (11 :: Word8)
  put Subtract = do
    put (12 :: Word8)
  put Multiply = do
    put (13 :: Word8)
  put Divide = do
    put (14 :: Word8)
  put Mod = do
    put (15 :: Word8)
  put Equal = do
    put (16 :: Word8)
  put LessThan = do
    put (17 :: Word8)
  put GreaterThan = do
    put (18 :: Word8)
  put Negate = do
    put (19 :: Word8)
  put (BuildList v) = do
    put (20 :: Word8)
    put v
  get = do
    inst <- get :: Get Word8
    case inst of
      0  -> Push <$> get
      1  -> return Pop
      2  -> Store <$> get
      3  -> StoreGlobal <$> get
      4  -> Load <$> get
      5  -> LoadGlobal <$> get
      6  -> Call <$> get
      7  -> JmpForward <$> get
      8  -> JmpBackward <$> get
      9  -> PopJmpIfFalse <$> get
      10 -> return Return
      11 -> return Add
      12 -> return Subtract
      13 -> return Multiply
      14 -> return Divide
      15 -> return Mod
      16 -> return Equal
      17 -> return LessThan
      18 -> return GreaterThan
      19 -> return Negate
      20 -> BuildList <$> get
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
