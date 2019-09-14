module Nuri.Pretty where

import           Data.Text.Prettyprint.Doc                ( Pretty
                                                          , Doc
                                                          , pretty
                                                          , vsep
                                                          , nest
                                                          , dquotes
                                                          , parens
                                                          , align
                                                          , (<+>)
                                                          )

import           Nuri.Literal
import           Nuri.Expr

instance Pretty Literal where
  pretty (LitInteger v) = pretty v
  pretty (LitReal    v) = pretty v
  pretty (LitChar    v) = pretty v
  pretty (LitBool    v) = pretty v

instance Pretty Op where
  pretty Plus             = "(+)"
  pretty Minus            = "(-)"
  pretty Asterisk         = "(*)"
  pretty Slash            = "(/)"
  pretty Percent          = "(%)"
  pretty Equal            = "(=)"
  pretty Inequal          = "(!=)"
  pretty LessThan         = "(<)"
  pretty GreaterThan      = "(>)"
  pretty LessThanEqual    = "(<=)"
  pretty GreaterThanEqual = "(>=)"


nest' :: Doc a -> Doc a
nest' = nest 4

instance Pretty Expr where
  pretty (Lit _ l        ) = pretty l
  pretty (Var _ ident    ) = "Var" <+> parens ((dquotes . pretty) ident)
  pretty (App _ func args) = nest' $ vsep
    [ "App"
    , "[func]" <+> pretty func
    , "[args]" <+> align (vsep (pretty <$> args))
    ]
  pretty (Assign _ ident val) =
    nest' $ vsep ["Assign", "[var]" <+> pretty ident, "[val]" <+> pretty val]
  pretty (BinaryOp _ op lhs rhs) = nest' $ vsep
    [ "BinaryOp"
    , "[op]" <+> pretty op
    , "[lhs]" <+> pretty lhs
    , "[rhs]" <+> pretty rhs
    ]
  pretty (UnaryOp _ op val) =
    nest' $ vsep ["UnaryOp", "[op]" <+> pretty op, "[val]" <+> pretty val]
