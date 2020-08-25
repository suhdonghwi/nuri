module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Text.Megaparsec

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input = runParser (scn *> parser <* scn <* eof) "(test)" input
