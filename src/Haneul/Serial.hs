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
                                                          , Pos
                                                          )

import           Haneul.Constant
import           Haneul.Instruction
import           Haneul.Program

-- instance Binary BuilderInternal where
--   put v = do
--     put (toList $ view internalConstTable v)
--   get =
--     BuilderInternal
--       <$> (fromList <$> get)
--       <*> pure S.empty
--       <*> pure 0
--       <*> pure []

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
  put (ConstChar v) = do
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
      3 -> ConstChar <$> get
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

instance (Binary a) => Binary (Instruction' a) where
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
  put PopName = do
    put (4 :: Word8)
  put (Call v) = do
    put (5 :: Word8)
    put v
  put (Jmp v) = do
    put (6 :: Word8)
    put v
  put (PopJmpIfFalse v) = do
    put (7 :: Word8)
    put v
  put Return = do
    put (8 :: Word8)
  put Add = do
    put (9 :: Word8)
  put Subtract = do
    put (10 :: Word8)
  put Multiply = do
    put (11 :: Word8)
  put Divide = do
    put (13 :: Word8)
  put Mod = do
    put (14 :: Word8)
  put Equal = do
    put (15 :: Word8)
  put LessThan = do
    put (16 :: Word8)
  put GreaterThan = do
    put (17 :: Word8)
  put Negate = do
    put (18 :: Word8)
  get = do
    inst <- get :: Get Word8
    case inst of
      0  -> Push <$> get
      1  -> return Pop
      2  -> Store <$> get
      4  -> Load <$> get
      5  -> return PopName
      6  -> Call <$> get
      7  -> Jmp <$> get
      8  -> PopJmpIfFalse <$> get
      9  -> return Return
      10 -> return Add
      11 -> return Subtract
      12 -> return Multiply
      13 -> return Divide
      14 -> return Mod
      15 -> return Equal
      16 -> return LessThan
      17 -> return GreaterThan
      18 -> return Negate
      _  -> fail "invalid instruction opcode type"

instance Binary Pos  where
  put v = do
    put (fromIntegral (unPos v) :: Word32)
  get = do
    line <- get :: Get Word32
    return (mkPos $ fromIntegral line)

instance Binary Program where
  put p = do
    put (toList $ view programConstTable p)
    put (view programCode p)
  get = liftA2 Program (fromList <$> get) get
