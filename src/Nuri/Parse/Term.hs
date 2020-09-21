module Nuri.Parse.Term where

import Nuri.Expr (Expr (..))
import Nuri.Literal
  ( Literal (..),
  )
import Nuri.Parse
  ( 
    MonadParser,
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

parseTerm :: (MonadParser m) => m Expr -> m Expr
parseTerm = lexeme . parseNonLexemeTerm

parseNonLexemeTerm :: (MonadParser m) => m Expr -> m Expr
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
    <|> parseParenCall parseExpr
    <|> parseParens parseExpr

toListStruct :: P.SourcePos -> [Expr] -> Expr
toListStruct pos [] = Lit pos LitNone
toListStruct pos (x : xs) = Struct pos "목록" [("첫번째", x), ("나머지", toListStruct pos xs)]

parseList  :: (MonadParser m) => m Expr -> m Expr
parseList expr = do
  pos <- P.getSourcePos
  elements <- brackets (expr `P.sepBy` symbol ",")
  return $ toListStruct pos elements
  where
    brackets = P.between (symbol "{") (symbol "}")

parseParenCall  :: (MonadParser m) => m Expr -> m Expr
parseParenCall expr = do
  pos <- P.getSourcePos
  funcName <- P.try $ funcIdentifier <* (P.char '(' >> sc)
  args <- parseArguments <* (sc >> P.char ')')
  return $ FuncCall pos (Var pos funcName) args
  where
    parseArgument = (,"_") <$> expr
    parseArguments = parseArgument `P.sepBy` (symbol ",")

parseStruct  :: (MonadParser m) => m Expr -> m Expr
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

parseParens  :: (MonadParser m) => m Expr -> m Expr
parseParens expr = P.between (P.char '(' >> sc) (sc >> P.char ')') expr

parseStringExpr  :: (MonadParser m) => m Expr
parseStringExpr = do
  pos <- P.getSourcePos
  let parseCh = do
        p <- P.getSourcePos
        char <- P.notFollowedBy (P.char '"') >> L.charLiteral
        return $ Lit p (LitChar char)
  toListStruct pos <$> P.between (P.char '"') (P.char '"') (P.many parseCh)

parseIdentifierExpr  :: (MonadParser m) => m Expr
parseIdentifierExpr = do
  pos <- P.getSourcePos
  ident <- parseIdentifier
  return $ Var pos ident

parseIdentifier  :: (MonadParser m) => m Text
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

parseNoneExpr  :: (MonadParser m) => m Expr
parseNoneExpr = do
  pos <- P.getSourcePos
  reserved "없음"
  return $ Lit pos LitNone

parseIntegerExpr  :: (MonadParser m) => m Expr
parseIntegerExpr = do
  pos <- P.getSourcePos
  val <- zeroNumber <|> parseDecimal
  return $ Lit pos (LitInteger val)
  where
    zeroNumber =
      P.char '0' >> parseHexadecimal <|> parseOctal <|> parseBinary <|> return 0

parseRealExpr  :: (MonadParser m) => m Expr
parseRealExpr = Lit <$> P.getSourcePos <*> (LitReal <$> parseReal)

parseCharExpr  :: (MonadParser m) => m Expr
parseCharExpr = Lit <$> P.getSourcePos <*> (LitChar <$> parseChar)

parseBoolExpr  :: (MonadParser m) => m Expr
parseBoolExpr = Lit <$> P.getSourcePos <*> (LitBool <$> parseBool)

parseBinary  :: (MonadParser m) => m Int64
parseBinary = P.char' 'b' >> (L.binary <?> "2진수")

parseOctal  :: (MonadParser m) => m Int64
parseOctal = L.octal <?> "8진수"

parseDecimal  :: (MonadParser m) => m Int64
parseDecimal = L.decimal <?> "정수"

parseHexadecimal  :: (MonadParser m) => m Int64
parseHexadecimal = P.char' 'x' >> (L.hexadecimal <?> "16진수")

parseReal  :: (MonadParser m) => m Double
parseReal = L.float

parseChar  :: (MonadParser m) => m Char
parseChar =
  ( P.between
      (P.char '\'')
      (P.char '\'')
      (P.notFollowedBy (P.char '\'') *> L.charLiteral)
  )
    <?> "문자"

parseBool :: (MonadParser m) => m Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")
