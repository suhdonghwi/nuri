{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haneul.Serial where

import Control.Lens ((^.))
import Data.Binary
  ( Binary (get, put),
    Get,
    Put,
    getWord8,
    putWord8,
  )
import Data.Binary.Get (getDoublebe)
import Data.Binary.Put (putDoublebe)
import Data.Char
  ( chr,
    ord,
  )
import Data.Set.Ordered (fromList)
import Data.Text (unpack)
import Haneul.Constant
  ( Constant (..),
    FuncObject (..),
    funcCode,
    funcConstTable,
    funcFilePath,
    funcGlobalVarNames,
    funcJosa,
    funcLineNo,
    funcLineNoTable,
    funcMaxLocalCount,
    funcName,
    funcStackSize,
  )
import Haneul.Instruction (Instruction' (..))
import Text.Megaparsec.Pos
  ( Pos,
    mkPos,
    unPos,
  )
import Prelude hiding
  ( fromList,
    get,
    put,
  )

instance Binary Constant where
  put ConstNone = do
    putWord8 0
  put (ConstInteger v) = do
    putWord8 1
    put v
  put (ConstReal v) = do
    putWord8 2
    putDoublebe v
  put (ConstChar v) = do
    putWord8 3
    put (fromIntegral (ord v) :: Word32)
  put (ConstBool v) = do
    putWord8 4
    put v
  put (ConstFunc v) = do
    putWord8 5
    put v
  put (ConstStruct v) = do
    putWord8 6
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
      6 -> ConstStruct <$> get
      _ -> fail "invalid constant type"

putWord8List :: (a -> Put) -> [a] -> Put
putWord8List f l = do
  putWord8 (genericLength l)
  sequence_ (f <$> l)

putJosa :: Text -> Put
putJosa = putWord8List (put :: Char -> Put) . unpack

putJosaList :: [Text] -> Put
putJosaList = putWord8List putJosa

instance Binary FuncObject where
  put obj = do
    putJosaList (obj ^. funcJosa)
    put (unpack <$> toList (obj ^. funcGlobalVarNames))
    put (obj ^. funcStackSize)
    put (obj ^. funcMaxLocalCount)
    put (toList $ obj ^. funcConstTable)
    put (unpack (obj ^. funcName))
    put (obj ^. funcFilePath)
    put (obj ^. funcLineNo)
    put (obj ^. funcLineNoTable)
    put (obj ^. funcCode)
  get = do
    josa <- get
    globalVarNames <- fromList <$> get
    maxStackSize <- get
    maxLocalCount <- get
    constTable <- fromList <$> get
    name <- get
    filePath <- get
    lineNo <- get
    lineNoTable <- get
    code <- get
    return
      ( FuncObject
          { _funcJosa = josa,
            _funcGlobalVarNames = globalVarNames,
            _funcCode = code,
            _funcConstTable = constTable,
            _funcName = name,
            _funcFilePath = filePath,
            _funcMaxLocalCount = maxLocalCount,
            _funcLineNo = lineNo,
            _funcLineNoTable = lineNoTable,
            _funcStackSize = maxStackSize
          }
      )

instance (Binary a) => Binary (Instruction' a) where
  put (Push v) = do
    putWord8 0
    put v
  put Pop = do
    putWord8 1
  put (LoadLocal v) = do
    putWord8 2
    put v
  put (StoreLocal v) = do
    putWord8 3
    put v
  put (LoadDeref v) = do
    putWord8 4
    put v
  put (StoreGlobal v) = do
    putWord8 5
    put v
  put (LoadGlobal v) = do
    putWord8 6
    put v
  put (Call v) = do
    putWord8 7
    putJosaList v
  put (AddStruct v1 v2) = do
    putWord8 8
    putJosa v1
    putJosaList v2
  put (MakeStruct v1 v2) = do
    putWord8 8
    putJosa v1
    putJosaList v2
  put (GetField v) = do
    putWord8 9
    putJosa v
  put (Jmp v) = do
    putWord8 10
    put v
  put (PopJmpIfFalse v) = do
    putWord8 11
    put v
  put (FreeVar v) = do
    putWord8 12
    putWord8List put v
  put Add = do
    putWord8 13
  put Subtract = do
    putWord8 14
  put Multiply = do
    putWord8 15
  put Divide = do
    putWord8 16
  put Mod = do
    putWord8 17
  put Equal = do
    putWord8 18
  put LessThan = do
    putWord8 19
  put GreaterThan = do
    putWord8 20
  put Negate = do
    putWord8 21
  put LogicNot = do
    putWord8 22
  put LogicAnd = do
    putWord8 23
  put LogicOr = do
    putWord8 24
  get = do
    inst <- get :: Get Word8
    let getterList =
          [ Push <$> get,
            return Pop,
            LoadLocal <$> get,
            StoreLocal <$> get,
            LoadDeref <$> get,
            StoreGlobal <$> get,
            LoadGlobal <$> get,
            Call <$> get,
            AddStruct <$> get <*> get,
            MakeStruct <$> get <*> get,
            GetField <$> get,
            Jmp <$> get,
            PopJmpIfFalse <$> get,
            FreeVar <$> get,
            return Add,
            return Subtract,
            return Multiply,
            return Divide,
            return Mod,
            return Equal,
            return LessThan,
            return GreaterThan,
            return Negate,
            return LogicNot,
            return LogicAnd,
            return LogicOr
          ]
    case getterList !!? (fromIntegral inst) of
      Just action -> action
      Nothing -> fail $ "invalid instruction opcode type" ++ show inst

instance Binary Pos where
  put v = do
    put (fromIntegral (unPos v) :: Word16)
  get = do
    line <- get :: Get Word16
    return (mkPos $ fromIntegral line)