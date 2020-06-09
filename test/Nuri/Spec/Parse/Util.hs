module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Nuri.Expr
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) initState
  where
    initState =
      [ FuncDecl pos1 VerbDecl "더하다" [("값1", "과"), ("값2", "를")] (Var pos1 "값1"),
        FuncDecl pos1 VerbDecl "합 구하다" [("값1", "과"), ("값2", "를")] (Var pos1 "값1"),
        FuncDecl pos1 VerbDecl "나누다" [("값1", "을"), ("값2", "로")] (Var pos1 "값1"),
        FuncDecl pos1 VerbDecl "들다" [("값", "을")] (Var pos1 "값1")
      ]
