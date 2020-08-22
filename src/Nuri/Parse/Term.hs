module Nuri.Parse.Term where

import Nuri.Expr (Expr (..))
import Nuri.Literal
  ( Literal (..),
  )
import Nuri.Parse
  ( Parser,
    getSourceLine,
    hangulJamo,
    hangulSyllable,
    lexeme,
    reserved,
    sc,
    symbol,
  )
import Nuri.Parse.Util (funcIdentifier)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

parseTerm :: Parser Expr -> Parser Expr
parseTerm = lexeme . parseNonLexemeTerm

parseNonLexemeTerm :: Parser Expr -> Parser Expr
parseNonLexemeTerm parseExpr =
  parseNoneExpr
    <|> parseBoolExpr
    <|> parseCharExpr
    <|> P.try (parseRealExpr)
    <|> parseIntegerExpr
    <|> parseIdentifierExpr
    <|> P.try (parseParenCall parseExpr)
    <|> parseParens parseExpr

parseParenCall :: Parser Expr -> Parser Expr
parseParenCall expr = do
  pos <- getSourceLine
  funcName <- funcIdentifier
  args <- parseParens parseArguments
  return $ FuncCall pos (Var pos funcName) ((,"_") <$> args)
  where
    parseArguments = expr `P.sepBy` (symbol ",")

parseParens :: Parser a -> Parser a
parseParens expr = P.between (P.char '(' >> sc) (sc >> P.char ')') expr

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = do
  pos <- getSourceLine
  ident <- parseIdentifier
  return $ Var pos ident

parseIdentifier :: Parser Text
parseIdentifier =
  ( P.between
      (P.char '[')
      (P.char ']')
      ( toText
          <$> liftA2
            (++)
            (P.some allowedChars)
            (P.many (P.char ' ' <|> allowedChars <|> (P.digitChar <?> "숫자")))
      )
  )
    <?> "변수 이름"
  where
    allowedChars = (hangulSyllable <|> hangulJamo <|> P.letterChar) <?> "한글 또는 영문"

parseNoneExpr :: Parser Expr
parseNoneExpr = do
  pos <- getSourceLine
  reserved "없음"
  return $ Lit pos LitNone

parseIntegerExpr :: Parser Expr
parseIntegerExpr = do
  pos <- getSourceLine
  val <- zeroNumber <|> parseDecimal
  return $ Lit pos (LitInteger val)
  where
    zeroNumber =
      P.char '0' >> parseHexadecimal <|> parseOctal <|> parseBinary <|> return 0

parseRealExpr :: Parser Expr
parseRealExpr = Lit <$> getSourceLine <*> (LitReal <$> parseReal)

parseCharExpr :: Parser Expr
parseCharExpr = Lit <$> getSourceLine <*> (LitChar <$> parseChar)

parseBoolExpr :: Parser Expr
parseBoolExpr = Lit <$> getSourceLine <*> (LitBool <$> parseBool)

parseBinary :: Parser Int64
parseBinary = P.char' 'b' >> (L.binary <?> "2진수")

parseOctal :: Parser Int64
parseOctal = L.octal <?> "8진수"

parseDecimal :: Parser Int64
parseDecimal = L.decimal <?> "정수"

parseHexadecimal :: Parser Int64
parseHexadecimal = P.char' 'x' >> (L.hexadecimal <?> "16진수")

parseReal :: Parser Double
parseReal = L.float

parseChar :: Parser Char
parseChar =
  ( P.between
      (P.char '\'')
      (P.char '\'')
      (P.notFollowedBy (P.char '\'') *> L.charLiteral)
  )
    <?> "문자"

parseBool :: Parser Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")