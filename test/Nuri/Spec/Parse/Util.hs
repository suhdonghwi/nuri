module Nuri.Spec.Parse.Util where

import Nuri.Parse
import Test.Hspec (expectationFailure)
import qualified Test.Hspec.Megaparsec as P
import Text.Megaparsec

testParse :: Parser a -> Text -> IO (Either (ParseErrorBundle Text Void) a)
testParse parser input = runParserT (scn *> parser <* scn <* eof) "(test)" input

shouldParse :: (ShowErrorComponent e, Stream s, Show a, Eq a) => IO (Either (ParseErrorBundle s e) a) -> a -> IO ()
shouldParse p e = do
  r <- p
  r `P.shouldParse` e

shouldFailOn :: (t -> IO (Either a b)) -> t -> IO ()
shouldFailOn p e = do
  r <- p e
  case r of
    Left _ -> pass
    Right _ -> expectationFailure "this parser should fail, but succeeded"