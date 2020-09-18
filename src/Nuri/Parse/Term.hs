module Nuri.Parse.Term where

import Nuri.Expr (Expr (..))
import Nuri.Literal
  ( Literal (..),
  )
import Nuri.Parse
  ( Parser,
    hangulJamo,
    hangulSyllable,
    lexeme,
    reserved,
    sc,
    scn,
    symbol,
  )
import Nuri.Parse.Util (funcIdentifier, structIdentifier)
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
    <|> parseStringExpr
    <|> parseList parseExpr
    <|> P.try (parseStruct parseExpr)
    <|> parseParens parseExpr

toListStruct :: P.SourcePos -> [Expr] -> Expr
toListStruct pos [] = Lit pos LitNone
toListStruct pos (x : xs) = Struct pos "목록" [("첫번째", x), ("나머지", toListStruct pos xs)]

parseList :: Parser Expr -> Parser Expr
parseList expr = do
  pos <- P.getSourcePos
  elements <- brackets (expr `P.sepBy` symbol ",")
  return $ toListStruct pos elements
  where
    brackets = P.between (symbol "{") (symbol "}")

parseStruct :: Parser Expr -> Parser Expr
parseStruct expr = do
  pos <- P.getSourcePos
  structName <- P.try $ structIdentifier <* (P.char '(' >> sc)
  args <- parseArguments <* (sc >> P.char ')')
  return $ Struct pos structName args
  where
    parseField = do
      argName <- P.try $ (funcIdentifier <?> "인자 이름") <* symbol ":"
      value <- expr
      return (argName, value)

    parseArguments = parseField `P.sepBy` (symbol "," <* scn)

parseParens :: Parser a -> Parser a
parseParens expr = P.between (P.char '(' >> sc) (sc >> P.char ')') expr

parseStringExpr :: Parser Expr
parseStringExpr = do
  pos <- P.getSourcePos
  let parseCh = do
        p <- P.getSourcePos
        char <- P.notFollowedBy (P.char '"') >> L.charLiteral
        return $ Lit p (LitChar char)
  toListStruct pos <$> P.between (P.char '"') (P.char '"') (P.many parseCh)

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = do
  pos <- P.getSourcePos
  ident <- parseIdentifier
  return $ Var pos ident

parseIdentifier :: Parser Text
parseIdentifier =
  ( P.between
      (P.char '[')
      (P.char ']')
      (toText <$> liftA2 (:) (firstChar) (P.many laterChar))
  )
    <?> "변수 이름"
  where
    firstChar = (hangulSyllable <|> hangulJamo <|> P.letterChar) <?> "한글 또는 영문"
    laterChar = firstChar <|> P.char ' ' <|> (P.digitChar <?> "숫자")

parseNoneExpr :: Parser Expr
parseNoneExpr = do
  pos <- P.getSourcePos
  reserved "없음"
  return $ Lit pos LitNone

parseIntegerExpr :: Parser Expr
parseIntegerExpr = do
  pos <- P.getSourcePos
  val <- zeroNumber <|> parseDecimal
  return $ Lit pos (LitInteger val)
  where
    zeroNumber =
      P.char '0' >> parseHexadecimal <|> parseOctal <|> parseBinary <|> return 0

parseRealExpr :: Parser Expr
parseRealExpr = Lit <$> P.getSourcePos <*> (LitReal <$> parseReal)

parseCharExpr :: Parser Expr
parseCharExpr = Lit <$> P.getSourcePos <*> (LitChar <$> parseChar)

parseBoolExpr :: Parser Expr
parseBoolExpr = Lit <$> P.getSourcePos <*> (LitBool <$> parseBool)

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
