module Nuri.Spec.Parse.Util where

import           Text.Megaparsec

import           Nuri.Parse

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input =
  evalState (runParserT (unParse parser) "(test)" input) []

testParse'
  :: Parser a -> Text -> Either (ParseErrorBundle Text Void) (a, [Text])
testParse' parser input =
  let (result, st) = runState (runParserT (unParse parser) "(test)" input) []
  in  (, st) <$> result
