{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           Nuri.Stmt

instance Pretty Literal where
  pretty (LitInteger v) = "LitInteger" <> parens (pretty v)
  pretty (LitReal    v) = "LitReal" <> parens (pretty v)
  pretty (LitString  v) = "LitString" <> parens (pretty v)
  pretty (LitBool    v) = "LitBool" <> parens (pretty v)

instance Pretty BinaryOperator where
  pretty Add              = "(+)"
  pretty Subtract         = "(-)"
  pretty Multiply         = "(*)"
  pretty Divide           = "(/)"
  pretty Mod              = "(%)"
  pretty Equal            = "(=)"
  pretty Inequal          = "(!=)"
  pretty LessThan         = "(<)"
  pretty GreaterThan      = "(>)"
  pretty LessThanEqual    = "(<=)"
  pretty GreaterThanEqual = "(>=)"

instance Pretty UnaryOperator where
  pretty Positive = "(+)"
  pretty Negative = "(-)"

nest' :: Doc a -> Doc a
nest' = nest 4

instance Pretty Expr where
  pretty (Lit _ l             ) = pretty l
  pretty (Var _ ident         ) = "Var" <+> parens ((dquotes . pretty) ident)
  pretty (FuncCall _ func args) = nest' $ vsep
    [ "App"
    , "[func]" <+> pretty func
    , "[args]" <+> align (vsep (pretty <$> args))
    ]
  pretty (BinaryOp _ op lhs rhs) = nest' $ vsep
    [ "BinaryOp"
    , "[op]" <+> pretty op
    , "[lhs]" <+> pretty lhs
    , "[rhs]" <+> pretty rhs
    ]
  pretty (UnaryOp _ op val) =
    nest' $ vsep ["UnaryOp", "[op]" <+> pretty op, "[val]" <+> pretty val]

instance Pretty Stmt where
  pretty (ExprStmt expr) = nest' $ vsep ["ExprStmt", "[expr]" <+> pretty expr]
  pretty (Return   expr) = nest' $ vsep ["Return", "[expr]" <+> pretty expr]
  pretty (Assign _ ident val) =
    nest' $ vsep ["Assign", "[var]" <+> pretty ident, "[val]" <+> pretty val]
  pretty (If _ condition thenStmts elseStmts) = nest' $ vsep
    [ "If"
    , "[condition]" <+> pretty condition
    , "[then]" <+> pretty thenStmts
    , "[else]" <+> pretty elseStmts
    ]
  pretty (While _ condition body) = nest' $ vsep
    ["While", "[condition]" <+> pretty condition, "[body]" <+> pretty body]
  pretty (FuncDecl _ name args body) = nest' $ vsep
    [ "FuncDecl"
    , "[name]" <+> pretty name
    , "[args]" <+> pretty args
    , "[body]" <+> pretty body
    ]
