module Nuri.Parse.Expr where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators

import           Nuri.Parse

binary :: Parser Integer
binary = string "0b" >> L.binary

octal :: Parser Integer
octal = string "0" >> L.octal

decimal :: Parser Integer
decimal = L.decimal

hexadecimal :: Parser Integer
hexadecimal = string "0x" >> L.hexadecimal
