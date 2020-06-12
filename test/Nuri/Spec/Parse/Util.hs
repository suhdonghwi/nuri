module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Nuri.Expr
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) initState
  where
    initState =
      [ (VerbDecl, "더하다"),
        (VerbDecl, "나누다"),
        (VerbDecl, "합 구하다"),
        (VerbDecl, "던지다"),
        (VerbDecl, "받다"),
        (VerbDecl, "보여주다"),
        (AdjectiveDecl, "같다"),
        (AdjectiveDecl, "크다"),
        (AdjectiveDecl, "작다")
      ]
