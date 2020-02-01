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
import           Nuri.Decl

instance Pretty Literal where
  pretty (LitInteger v) = "LitInteger" <> parens (pretty v)
  pretty (LitReal    v) = "LitReal" <> parens (pretty v)
  pretty (LitString  v) = "LitString" <> parens (pretty v)
  pretty (LitBool    v) = "LitBool" <> parens (pretty v)
  pretty LitNone        = "LitNone"

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
  pretty (Lit _ lit           ) = pretty lit
  pretty (Var _ ident         ) = "Var" <+> parens ((dquotes . pretty) ident)
  pretty (FuncCall _ func args) = nest'
    $ vsep ["App", "[func]" <+> pretty func, "[args]" <+> align (pretty args)]
  pretty (If _ cond thenExpr elseExpr) = nest' $ vsep
    [ "If"
    , "[cond]" <+> pretty cond
    , "[then]" <+> pretty thenExpr
    , "[else]" <+> pretty elseExpr
    ]
  pretty (BinaryOp _ op lhs rhs) = nest' $ vsep
    [ "BinaryOp"
    , "[op]" <+> pretty op
    , "[lhs]" <+> pretty lhs
    , "[rhs]" <+> pretty rhs
    ]
  pretty (UnaryOp _ op val) =
    nest' $ vsep ["UnaryOp", "[op]" <+> pretty op, "[val]" <+> pretty val]
  pretty (List _ list) = nest' $ vsep ["List", "[elements]" <+> pretty list]
  pretty (Seq exprs  ) = nest' $ vsep ["Seq", pretty exprs]
  pretty (Lambda _ args body) =
    nest' $ vsep ["Lambda", "[args]" <+> pretty args, "[body]" <+> pretty body]
  pretty (Let _ name value expr) = nest' $ vsep
    [ "Let"
    , "[name]" <+> pretty name
    , "[value]" <+> pretty value
    , "[expr]" <+> pretty expr
    ]

instance Pretty Stmt where
  pretty (DeclStmt decl) = nest' $ vsep ["DeclStmt", pretty decl]

instance Pretty Decl where
  pretty (FuncDecl _ name args body) = nest' $ vsep
    [ "FuncDecl"
    , "[name]" <+> pretty name
    , "[args]" <+> pretty args
    , "[body]" <+> pretty body
    ]
