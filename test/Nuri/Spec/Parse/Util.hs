module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Nuri.Expr
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) initState
  where
    initState =
      [ (VerbDecl, "더하다"),
        (VerbDecl, "합 구하다"),
        (VerbDecl, "나누다"),
        (VerbDecl, "들다"),
        (AdjectiveDecl, "같다")
      ]
