module Nuri.Parse.PartTable where

import Nuri.Expr (DeclKind)
import Data.Map (insert, lookup)

type PartTable = Map Text DeclKind
type MonadPartTable m = (MonadState PartTable m)

addDecl :: (MonadState PartTable m) => Text -> DeclKind -> m ()
addDecl ident kind = modify (insert ident kind)

lookupDecl :: (MonadState PartTable m) => Text -> m (Maybe DeclKind)
lookupDecl ident = lookup ident <$> get

