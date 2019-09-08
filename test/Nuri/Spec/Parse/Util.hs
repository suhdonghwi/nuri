module Nuri.Spec.Parse.Util where

import           Text.Megaparsec

import qualified Data.Set                      as S

import           Nuri.Parse

testParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
testParse parser input =
  evalState (runParserT (unParse parser) "(test)" input) S.empty

testParse'
  :: Parser a -> Text -> Either (ParseErrorBundle Text Void) (a, Set Text)
testParse' parser input =
  let (result, st) =
          runState (runParserT (unParse parser) "(test)" input) S.empty
  in  (, st) <$> result
