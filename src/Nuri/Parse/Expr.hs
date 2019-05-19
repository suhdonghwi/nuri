module Nuri.Parse.Expr where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators

import Nuri.Parse

parseBinary :: Parser Integer
parseBinary = L.binary

parseOctal :: Parser Integer
parseOctal = L.octal

parseDecimal :: Parser Integer
parseDecimal = L.decimal

parseHexadecimal :: Parser Integer
parseHexadecimal = L.hexadecimal