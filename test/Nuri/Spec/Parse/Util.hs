module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = evalState (runParserT (scn *> parser <* scn <* eof) "(test)" input) []
