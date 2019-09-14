module Nuri.Pretty where

import           Data.Text.Prettyprint.Doc                ( Pretty
                                                          , pretty
                                                          )

import           Nuri.Literal

instance Pretty Literal where
  pretty (LitInteger v) = pretty v
  pretty (LitReal    v) = pretty v
  pretty (LitChar    v) = pretty v
  pretty (LitBool    v) = pretty v
