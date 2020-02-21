{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haneul.Serial where

import           Prelude                           hiding ( put
                                                          , get
                                                          , fromList
                                                          )

import           Data.Binary                              ( Binary(put, get)
                                                          , Get
                                                          , putWord8
                                                          , getWord8
                                                          )
import           Data.Binary.Put                          ( putDoublebe )
import           Data.Binary.Get                          ( getDoublebe )
import           Data.Char                                ( ord
                                                          , chr
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
    putWord8 0
  put (ConstInteger v) = do
    putWord8 1
    put v
  put (ConstReal v) = do
    putWord8 2
    putDoublebe v
    -- let (base, e) = decodeFloat v
    -- put (fromIntegral base :: Int64)
    -- put e
  put (ConstChar v) = do
    putWord8 3
    put (fromIntegral (ord v) :: Word32)
  put (ConstBool v) = do
    putWord8 4
    put v
  put (ConstFunc v) = do
    putWord8 5
    put v
  get = do
    t <- getWord8
    case t of
      0 -> return ConstNone
      1 -> ConstInteger <$> get
      2 -> ConstReal <$> getDoublebe
      3 -> (ConstChar . chr) <$> get
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
    putWord8 0
    put v
  put Pop = do
    putWord8 1
  put (Load v) = do
    putWord8 2
    put v
  put (LoadDeref v) = do
    putWord8 3
    put v
  put (StoreGlobal v) = do
    putWord8 4
    put v
  put (LoadGlobal v) = do
    putWord8 5
    put v
  put (Call v) = do
    putWord8 6
    put v
  put (Jmp v) = do
    putWord8 7
    put v
  put (PopJmpIfFalse v) = do
    putWord8 8
    put v
  put (PushFreeVar v) = do
    putWord8 9
    put v
  put Add = do
    putWord8 10
  put Subtract = do
    putWord8 11
  put Multiply = do
    putWord8 12
  put Divide = do
    putWord8 13
  put Mod = do
    putWord8 14
  put Equal = do
    putWord8 15
  put LessThan = do
    putWord8 16
  put GreaterThan = do
    putWord8 17
  put Negate = do
    putWord8 18
  get = do
    inst <- get :: Get Word8
    let getterList =
          [ Push <$> get
          , return Pop
          , Load <$> get
          , LoadDeref <$> get
          , StoreGlobal <$> get
          , LoadGlobal <$> get
          , Call <$> get
          , Jmp <$> get
          , PopJmpIfFalse <$> get
          , PushFreeVar <$> get
          , return Add
          , return Subtract
          , return Multiply
          , return Divide
          , return Mod
          , return Equal
          , return LessThan
          , return GreaterThan
          , return Negate
          ]
    case getterList !!? (fromIntegral inst) of
      Just action -> action
      Nothing     -> fail $ "invalid instruction opcode type" ++ show inst

instance Binary Pos  where
  put v = do
    put (fromIntegral (unPos v) :: Word32)
  get = do
    line <- get :: Get Word32
    return (mkPos $ fromIntegral line)

instance Binary Program where
  put p = do
    put (view programGlobalVarNames p)
    put (toList $ view programConstTable p)
    put (view programCode p)
  get = liftA3 Program get (fromList <$> get) get
