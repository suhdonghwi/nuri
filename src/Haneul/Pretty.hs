{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haneul.Pretty where

import           Control.Lens                             ( view )

import           Data.Text.Prettyprint.Doc                ( Pretty
                                                          , Doc
                                                          , pretty
                                                          , vsep
                                                          , nest
                                                          , align
                                                          , (<+>)
                                                          )
import           Data.Set.Ordered                         ( OSet )

import           Haneul.Builder
import           Haneul.Constant
import           Haneul.Instruction

nest' :: Doc a -> Doc a
nest' = nest 4

instance (Show a, Pretty a) => Pretty (OSet a) where
  pretty set = pretty $ toList set

instance Pretty Constant where
  pretty (ConstFunc obj) = nest' $ vsep
    [ "ConstFunc"
    , "[arity]" <+> pretty (view funcArity obj)
    , "[const table]" <+> pretty (view funcConstTable obj)
    , "[var names]" <+> pretty (view funcVarNames obj)
    , "[body]" <+> (align . vsep) (pretty <$> view funcBody obj)
    ]
  pretty v = show v

instance Pretty BuilderInternal where
  pretty val = vsep
    [ "[const table]" <+> pretty (view internalConstTable val)
    , "[var names]" <+> pretty (view internalVarNames val)
    ]

instance Pretty Instruction where
  pretty = show

instance Pretty AnnInstruction where
  pretty inst =
    show (view lineNumber inst) <+> "|" <+> pretty (view instruction inst)
