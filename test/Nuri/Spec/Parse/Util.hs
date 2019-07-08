module Nuri.Spec.Parse.Util where

import Text.Megaparsec


testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse parser = parse parser "(test)"
