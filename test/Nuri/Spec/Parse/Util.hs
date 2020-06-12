module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Nuri.Expr
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) initState
  where
    initState =
      [ Decl pos1 "더하다" $ FuncDecl VerbDecl [("값1", "과"), ("값2", "를")] (Var pos1 "값1"),
        Decl pos1 "합 구하다" $ FuncDecl VerbDecl [("값1", "과"), ("값2", "를")] (Var pos1 "값1"),
        Decl pos1 "나누다" $ FuncDecl VerbDecl [("값1", "을"), ("값2", "로")] (Var pos1 "값1"),
        Decl pos1 "들다" $ FuncDecl VerbDecl [("값", "을")] (Var pos1 "값1"),
        Decl pos1 "같다" $ FuncDecl AdjectiveDecl [("값1", "와"), ("값2", "이")] (Var pos1 "값1")
      ]
