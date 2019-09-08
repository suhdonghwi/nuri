module Nuri.Spec.Parse.Util where

import           Text.Megaparsec

import           Nuri.Parse

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input =
  evalState (runParserT (unParse parser) "(test)" input) []
