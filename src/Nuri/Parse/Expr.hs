module Nuri.Parse.Expr where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.List                      ( foldl1' )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators
import           Control.Monad.Combinators.Expr

import           Nuri.Parse
import           Nuri.Expr

arithmetic = makeExprParser term table
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

nestedFuncCalls :: Parser Expr
nestedFuncCalls = do
  calls <- some funcCall
  let addArg arg (App pos func args) = App pos func (arg : args)
  return $ foldl1' addArg calls

funcCall :: Parser Expr
funcCall = do
  args <- many term
  pos  <- getSourcePos
  func <- funcIdentifier
  return $ App pos func args

funcIdentifier :: Parser Text
funcIdentifier = lexeme $ pack <$> some (oneOf ['가' .. '힣'])

term :: Parser Expr
term = integer <|> identifier

identifier :: Parser Expr
identifier = lexeme $ do
  pos <- getSourcePos
  let allowedChars =
        ['가' .. '힣']
          ++ ['ㄱ' .. 'ㅎ']
          ++ ['ㅏ' .. 'ㅣ']
          ++ ['a' .. 'z']
          ++ ['A' .. 'Z']
      identifierRule = (++) <$> some (oneOf allowedChars) <*> many
        (oneOf $ ' ' : allowedChars ++ ['0' .. '9'])
  char '['
  identStr <- pack <$> identifierRule
  char ']'
  return $ Var pos identStr

integer :: Parser Expr
integer = do
  pos   <- getSourcePos
  value <- zeroNumber <|> decimal
  return $ Lit pos (LitInteger value)
 where
  zeroNumber = do
    char '0'
    hexadecimal <|> octal <|> binary <|> return 0

binary :: Parser Integer
binary = lexeme $ char' 'b' >> L.binary

octal :: Parser Integer
octal = lexeme L.octal

decimal :: Parser Integer
decimal = lexeme L.decimal

hexadecimal :: Parser Integer
hexadecimal = lexeme $ char' 'x' >> L.hexadecimal
