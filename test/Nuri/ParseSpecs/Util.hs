module Nuri.ParseSpecs.Util where

import           Text.Megaparsec
import           Text.Megaparsec.Error

testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse parser input = parse parser "(test)" input
