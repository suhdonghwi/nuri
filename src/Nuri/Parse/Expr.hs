module Nuri.Parse.Expr where


import           Data.List                                ( foldl1' )

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                          ( (<?>) )
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
import           Nuri.Literal

parseExpr :: Parser Expr
parseExpr = parseArithmetic

parseArithmetic :: Parser Expr
parseArithmetic = makeExprParser
  (   (   P.try
          (  parseTerm
          <* P.notFollowedBy (void parseTerm <|> void parseFuncIdentifier) -- 후에 조사로 변경
          )
      <|> parseNestedFuncCalls
      )
  <?> "표현식"
  )
  table
 where
  table =
    [ [Prefix $ unaryOp "+" Positive, Prefix $ unaryOp "-" Negative]
    , [ InfixL $ binaryOp "*" Multiply
      , InfixL $ binaryOp "/" Divide
      , InfixL $ binaryOp "%" Mod
      ]
    , [InfixL $ binaryOp "+" Add, InfixL $ binaryOp "-" Subtract]
    , [InfixL $ binaryOp "==" Equal, InfixL $ binaryOp "!=" Inequal]
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
  calls <- P.sepBy1 (parseFuncCall <?> "함수 호출식") (symbol ",")
  let addArg arg (App pos func args) = App pos func (arg : args)
      addArg _   _                   = error "불가능한 상황"
  return $ foldl1' addArg calls

parseFuncCall :: Parser Expr
parseFuncCall = do
  args <- P.many (parseTerm <?> "함수 인수")
  pos  <- P.getSourcePos
  func <- parseFuncIdentifier <?> "함수 이름"
  return $ App pos (Var pos func) args

parseFuncIdentifier :: Parser Text
parseFuncIdentifier = lexeme
  (unwords <$> P.sepEndBy1 (P.try $ P.notFollowedBy keyword *> hangulWord)
                           (P.char ' ')
  )
 where
  keywords = ["반환하다", "함수", "참", "거짓", "만약", "이라면", "아니고", "아니면", "반복", "인 동안"]
  keyword = P.choice $ fmap reserved keywords
  hangulWord = toText <$> P.some hangulSyllable
    -- if word `elem` keywords then fail "예약어를 함수 이름으로 쓸 수 없습니다." else return word

parseTerm :: Parser Expr
parseTerm =
  parseBoolExpr
    <|> parseCharExpr
    <|> P.try parseRealExpr
    <|> parseIntegerExpr
    <|> parseIdentifierExpr
    <|> parseParens

parseParens :: Parser Expr
parseParens = P.between (symbol "(") (symbol ")") parseExpr

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = Var <$> P.getSourcePos <*> parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier =
  lexeme
      (toText <$> P.between
        (P.char '[')
        (P.char ']')
        ((++) <$> P.some allowedChars <*> P.many
          (P.char ' ' <|> allowedChars <|> (P.digitChar <?> "숫자"))
        )
      )
    <?> "변수 이름"
 where
  allowedChars = hangulSyllable <|> hangulJamo <|> (P.letterChar <?> "영문")

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
parseBinary = P.char' 'b' >> (L.binary <?> "2진수")

parseOctal :: Parser Integer
parseOctal = L.octal <?> "8진수"

parseDecimal :: Parser Integer
parseDecimal = L.decimal <?> "정수"

parseHexadecimal :: Parser Integer
parseHexadecimal = P.char' 'x' >> (L.hexadecimal <?> "16진수")

parseReal :: Parser Double
parseReal = lexeme L.float

parseChar :: Parser Char
parseChar = lexeme
  (P.between (P.char '\'' >> P.notFollowedBy (P.char '\''))
             (P.char '\'')
             (L.charLiteral <?> "문자")
  )

parseBool :: Parser Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")
