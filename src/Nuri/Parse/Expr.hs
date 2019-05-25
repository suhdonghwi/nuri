module Nuri.Parse.Expr where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators
import           Control.Monad.Combinators.Expr

import           Nuri.Parse
import           Nuri.Expr

arithmetic = makeExprParser integer table <?> "표현식"
 where
  table =
    [ [Prefix $ unaryOp "+" Plus, Prefix $ unaryOp "-" Minus]
    , [InfixL $ binaryOp "*" Asterisk, InfixL $ binaryOp "/" Slash]
    , [InfixL $ binaryOp "+" Plus, InfixL $ binaryOp "-" Minus]
    ]
  binaryOp opStr op = do
    pos <- getSourcePos
    (\l r -> BinaryOp pos op l r) <$ L.symbol sc opStr
  unaryOp opStr op = do
    pos <- getSourcePos
    (\v -> UnaryOp pos op v) <$ L.symbol sc opStr

integer :: Parser Expr
integer = do
  pos <- getSourcePos
  i   <- try binary <|> try hexadecimal <|> try octal <|> decimal
  return $ Lit pos (LitInteger i)

binary :: Parser Integer
binary = lexeme $ string' "0b" >> L.binary

octal :: Parser Integer
octal = lexeme $ string "0" >> L.octal

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme $ string' "0x" >> L.hexadecimal
