module Nuri.Parse.Expr where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators
import           Control.Monad.Combinators.Expr

import           Nuri.Parse

arithmetic = makeExprParser integer table <?> "표현식"
 where
  table =
    [[binaryOp "*" (*), binaryOp "/" div], [binaryOp "+" (+), binaryOp "-" (-)]]
  binaryOp op f = InfixL (f <$ L.symbol sc op)

integer :: Parser Integer
integer = try binary <|> try hexadecimal <|> try octal <|> decimal

binary :: Parser Integer
binary = lexeme $ string' "0b" >> L.binary

octal :: Parser Integer
octal = lexeme $ string "0" >> L.octal

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme $ string' "0x" >> L.hexadecimal
