module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Nuri.Expr
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) initState
  where
    initState =
      [ (NormalDecl, "더하다"),
        (NormalDecl, "나누다"),
        (NormalDecl, "합 구하다"),
        (NormalDecl, "던지다"),
        (NormalDecl, "받다"),
        (NormalDecl, "보여주다"),
        (AdjectiveDecl, "같다"),
        (AdjectiveDecl, "크다"),
        (AdjectiveDecl, "작다")
      ]
