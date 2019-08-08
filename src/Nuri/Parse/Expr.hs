module Nuri.Parse.Expr where


import           Data.Text                                ( Text
                                                          , pack
                                                          )
import           Data.List                                ( foldl1' )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators.Expr

import           Nuri.Parse
import           Nuri.Expr

parseExpr :: Parser Expr
parseExpr = parseArithmetic

parseArithmetic :: Parser Expr
parseArithmetic = makeExprParser (parseNestedFuncCalls <|> parseTerm) table
 where
  table =
    [ [Prefix $ unaryOp "+" Plus, Prefix $ unaryOp "-" Minus]
    , [ InfixL $ binaryOp "*" Asterisk
      , InfixL $ binaryOp "/" Slash
      , InfixL $ binaryOp "%" Percent
      ]
    , [InfixL $ binaryOp "+" Plus, InfixL $ binaryOp "-" Minus]
    , [InfixL $ binaryOp "=" Equal, InfixL $ binaryOp "!=" Inequal]
    ]
  binaryOp opStr op = do
    pos <- getSourcePos
    BinaryOp pos op <$ L.symbol sc opStr
  unaryOp opStr op = do
    pos <- getSourcePos
    UnaryOp pos op <$ L.symbol sc opStr

parseNestedFuncCalls :: Parser Expr
parseNestedFuncCalls = do
  calls <- some (try parseFuncCall)
  let addArg arg (App pos func args) = App pos func (arg : args)
      addArg _   _                   = undefined
  return $ foldl1' addArg calls

parseFuncCall :: Parser Expr
parseFuncCall = do
  args <- many parseTerm
  pos  <- getSourcePos
  func <- parseFuncIdentifier
  return $ App pos (Var pos func) args

parseFuncIdentifier :: Parser Text
parseFuncIdentifier = lexeme $ do
  ident <- pack <$> some hangulSyllable
  if ident `elem` keywords then fail "예약어를 함수 이름으로 쓸 수 없습니다." else return ident
 where
  keywords = ["반환하다", "돌려주다", "참", "거짓", "만약", "면", "이면", "이라면", "아니고", "아니면"]

parseTerm :: Parser Expr
parseTerm =
  try parseRealExpr
    <|> parseIntegerExpr
    <|> parseBoolExpr
    <|> try parseAssignment
    <|> parseIdentifierExpr
    <|> parseParens

parseParens :: Parser Expr
parseParens = between (symbol "(") (symbol ")") parseExpr

parseAssignment :: Parser Expr
parseAssignment = do
  pos         <- getSourcePos
  Var _ ident <- parseIdentifierExpr
  _           <- symbol ":"
  val         <- parseExpr
  return $ Assign pos ident val

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr =
  lexeme $ Var <$> getSourcePos <*> (char '[' >> parseIdentifier <* char ']')

parseIdentifier :: Parser Text
parseIdentifier =
  pack
    <$> ((++) <$> some allowedChars <*> many
          (char ' ' <|> allowedChars <|> digitChar)
        )
  where allowedChars = hangulSyllable <|> hangulJamo <|> letterChar

parseIntegerExpr :: Parser Expr
parseIntegerExpr = lexeme $ do
  pos <- getSourcePos
  val <- zeroNumber <|> parseDecimal
  return $ Lit pos (LitInteger val)
 where
  zeroNumber =
    char '0' >> parseHexadecimal <|> parseOctal <|> parseBinary <|> return 0

parseRealExpr :: Parser Expr
parseRealExpr = Lit <$> getSourcePos <*> (LitReal <$> parseReal)

parseBoolExpr :: Parser Expr
parseBoolExpr = Lit <$> getSourcePos <*> (LitBool <$> parseBool)

parseBinary :: Parser Integer
parseBinary = char' 'b' >> L.binary

parseOctal :: Parser Integer
parseOctal = L.octal

parseDecimal :: Parser Integer
parseDecimal = L.decimal

parseHexadecimal :: Parser Integer
parseHexadecimal = char' 'x' >> L.hexadecimal

parseReal :: Parser Double
parseReal = lexeme L.float

parseChar :: Parser Char
parseChar = between (symbol "\'" >> notFollowedBy (symbol "\'"))
                    (symbol "\'")
                    L.charLiteral

parseBool :: Parser Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")
