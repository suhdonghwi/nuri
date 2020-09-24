module Nuri.Parse.PartTable where

import Data.Map (insert, lookup)
import Nuri.Expr (FuncKind (..))
import qualified Text.Megaparsec as P

type PartTable = Map Text FuncKind

type MonadPartTable m = (MonadState PartTable m)

addDecl :: (MonadState PartTable m) => Text -> FuncKind -> m ()
addDecl ident kind = modify (insert ident kind)

lookupDecl :: (MonadState PartTable m) => Text -> m (Maybe FuncKind)
lookupDecl ident = lookup ident <$> get

checkExistence :: (P.MonadParsec Void Text m, MonadState PartTable m, MonadFail m) => Int -> Text -> m FuncKind
checkExistence offset ident = do
  result <- lookupDecl ident
  case result of
    Nothing -> do
      P.setOffset offset
      fail $ "함수 '" <> toString ident <> "'를 찾을 수 없습니다"
    Just kind -> return kind

checkFuncKind :: (P.MonadParsec Void Text m, MonadState PartTable m, MonadFail m) => Int -> Text -> FuncKind -> m ()
checkFuncKind offset ident kind = do
  result <- checkExistence offset ident
  if result == kind
    then pass
    else do
      P.setOffset offset
      fail $ "'" <> toString ident <> "' 라는 " <> declKindToString kind <> " 함수를 찾을 수 없습니다."
  where
    declKindToString :: FuncKind -> String
    declKindToString k = case k of
      Normal -> "일반"
      Verb -> "동사"
      Adjective -> "형용사"
