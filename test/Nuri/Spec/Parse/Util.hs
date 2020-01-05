module Nuri.Spec.Parse.Util where

import           Text.Megaparsec

import           Nuri.Parse

testParse :: Parser a -> String -> Either (ParseErrorBundle String Void) a
testParse parser = runParser parser "(test)"
