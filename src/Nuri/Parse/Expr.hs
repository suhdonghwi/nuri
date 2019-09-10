module Nuri.Parse.Expr where


import           Data.List                                ( foldl1' )

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

import qualified Text.Megaparsec.Char.Lexer    as L

import           Control.Monad.Combinators.Expr           ( makeExprParser
                                                          , Operator
                                                            ( Prefix
                                                            , InfixL
                                                            )
                                                          )

import           Nuri.Parse
import           Nuri.Expr

parseExpr :: Parser Expr
parseExpr = parseArithmetic

parseArithmetic :: Parser Expr
parseArithmetic = makeExprParser
  ((parseNestedFuncCalls <|> parseTerm) P.<?> "표현식")
  table
 where
  table =
    [ [Prefix $ unaryOp "+" Plus, Prefix $ unaryOp "-" Minus]
    , [ InfixL $ binaryOp "*" Asterisk
      , InfixL $ binaryOp "/" Slash
      , InfixL $ binaryOp "%" Percent
      ]
    , [InfixL $ binaryOp "+" Plus, InfixL $ binaryOp "-" Minus]
    , [InfixL $ binaryOp "=" Equal, InfixL $ binaryOp "!=" Inequal]
    , [ InfixL $ binaryOp "<=" LessThanEqual
      , InfixL $ binaryOp ">=" GreaterThanEqual
      , InfixL $ binaryOp "<" LessThan
      , InfixL $ binaryOp ">" GreaterThan
      ]
    ]
  binaryOp opStr op = P.hidden $ do
    pos <- P.getSourcePos
    BinaryOp pos op <$ L.symbol sc opStr
  unaryOp opStr op = P.hidden $ do
    pos <- P.getSourcePos
    UnaryOp pos op <$ L.symbol sc opStr

parseNestedFuncCalls :: Parser Expr
parseNestedFuncCalls = do
  calls <- P.sepBy1 (P.try parseFuncCall) (symbol ",")
  let addArg arg (App pos func args) = App pos func (arg : args)
      addArg _   _                   = error "불가능한 상황"
  return $ foldl1' addArg calls

parseFuncCall :: Parser Expr
parseFuncCall = do
  args <- P.many parseTerm
  pos  <- P.getSourcePos
  func <- parseFuncIdentifier
  return $ App pos (Var pos func) args

parseFuncIdentifier :: Parser Text
parseFuncIdentifier = lexeme $ unwords <$> P.sepEndBy1
  (P.try $ P.notFollowedBy keyword *> hangulWord)
  (P.char ' ')
 where
  keywords =
    ["반환하다", "참", "거짓", "만약", "면", "이면", "이라면", "아니고", "아니면", "인 동안 반복"]
  keyword    = P.choice $ fmap reserved keywords
  hangulWord = toText <$> P.some hangulSyllable
    -- if word `elem` keywords then fail "예약어를 함수 이름으로 쓸 수 없습니다." else return word

parseTerm :: Parser Expr
parseTerm =
  parseBoolExpr
    <|> parseCharExpr
    <|> P.try parseRealExpr
    <|> parseIntegerExpr
    <|> P.try parseAssignment
    <|> parseIdentifierExpr
    <|> parseParens

parseParens :: Parser Expr
parseParens = P.between (symbol "(") (symbol ")") parseExpr

parseAssignment :: Parser Expr
parseAssignment = do
  pos         <- P.getSourcePos
  Var _ ident <- parseIdentifierExpr
  _           <- symbol ":"
  Assign pos ident <$> parseExpr

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = Var <$> P.getSourcePos <*> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = lexeme $ toText <$> P.between
  (P.char '[')
  (P.char ']')
  ((++) <$> P.some allowedChars <*> P.many
    (P.char ' ' <|> allowedChars <|> P.digitChar)
  )
  where allowedChars = hangulSyllable <|> hangulJamo <|> P.letterChar

parseIntegerExpr :: Parser Expr
parseIntegerExpr = lexeme $ do
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

parseBinary :: Parser Integer
parseBinary = P.char' 'b' >> L.binary

parseOctal :: Parser Integer
parseOctal = L.octal

parseDecimal :: Parser Integer
parseDecimal = L.decimal

parseHexadecimal :: Parser Integer
parseHexadecimal = P.char' 'x' >> L.hexadecimal

parseReal :: Parser Double
parseReal = lexeme L.float

parseChar :: Parser Char
parseChar = lexeme
  (P.between (P.char '\'' >> P.notFollowedBy (P.char '\''))
             (P.char '\'')
             L.charLiteral
  )

parseBool :: Parser Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")
