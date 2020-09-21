module Nuri.Parse.PartTable where

import Nuri.Expr (DeclKind(..))
import Data.Map (insert, lookup)
import qualified Text.Megaparsec as P

type PartTable = Map Text DeclKind
type MonadPartTable m = (MonadState PartTable m)

addDecl :: (MonadState PartTable m) => Text -> DeclKind -> m ()
addDecl ident kind = modify (insert ident kind)

lookupDecl :: (MonadState PartTable m) => Text -> m (Maybe DeclKind)
lookupDecl ident = lookup ident <$> get

checkExistence :: (P.MonadParsec Void Text m, MonadState PartTable m, MonadFail m) => Int -> Text -> m DeclKind
checkExistence offset ident = do
  result <- lookupDecl ident
  case result of
    Nothing -> do
      P.setOffset offset
      fail $ "식별자 '" <> toString ident <> "'를 찾을 수 없습니다"
    Just kind -> return kind

checkDeclKind :: (P.MonadParsec Void Text m, MonadState PartTable m, MonadFail m) => Int -> Text -> DeclKind -> m ()
checkDeclKind offset ident kind = do
  result <- checkExistence offset ident
  if result == kind
     then pass
     else do
       P.setOffset offset
       fail $ "'" <> toString ident <> "' 라는 " <> declKindToString kind <> " 함수를 찾을 수 없습니다."
  where 
    declKindToString :: DeclKind -> String
    declKindToString k = case k of
                           NormalDecl -> "일반"
                           VerbDecl -> "동사"
                           AdjectiveDecl -> "형용사"
