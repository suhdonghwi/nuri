module Nuri.Parse.PartTable where

import Nuri.Expr (DeclKind)
import Data.Map (insert, lookup)

type PartTable = Map Text DeclKind

addDecl :: (MonadState PartTable m) => Text -> DeclKind -> m ()
addDecl ident kind = modify (insert ident kind)

lookupDecl :: (MonadState PartTable m) => Text -> m (Maybe DeclKind)
lookupDecl ident = lookup ident <$> get

