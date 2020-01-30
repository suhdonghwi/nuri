module Nuri.Parse.Expr where

import           Prelude                           hiding ( unwords )

import           Data.List                                ( foldl1' )
import           Data.String                              ( unwords )

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
parseExpr = parseExprChain <|> parseIf <|> parseArithmetic

parseExprChain :: Parser Expr
parseExprChain = do
  reserved "순서대로"
  P.newline
  scn
  level <- L.indentLevel
  Seq . fromList <$> P.some
    (do
      L.indentGuard scn EQ level
      parseExpr
    )

parseIf :: Parser Expr
parseIf =
  (do
      pos <- getSourceLine
      reserved "만약"
      condExpr <- parseArithmetic
      reserved "이라면"
      thenExpr <- parseArithmetic
      reserved "아니라면"
      If pos condExpr thenExpr <$> parseArithmetic
    )
    <?> "조건식"

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
    pos <- getSourceLine
    BinaryOp pos op <$ L.symbol sc opStr
  unaryOp opStr op = P.hidden $ do
    pos <- getSourceLine
    UnaryOp pos op <$ L.symbol sc opStr

parseNestedFuncCalls :: Parser Expr
parseNestedFuncCalls = do
  calls <- P.sepBy1 (parseFuncCall <?> "함수 호출식") (symbol ",")
  let addArg arg (FuncCall pos func args) = FuncCall pos func (arg : args)
      addArg _   _                        = error "불가능한 상황"
  return $ foldl1' addArg calls

parseFuncCall :: Parser Expr
parseFuncCall = do
  args <- P.many (parseTerm <?> "함수 인수")
  pos  <- getSourceLine
  func <- parseFuncIdentifier <?> "함수 이름"
  return $ FuncCall pos func args

parseFuncIdentifier :: Parser String
parseFuncIdentifier = lexeme
  (unwords <$> P.sepEndBy1 (P.try $ P.notFollowedBy keyword *> hangulWord)
                           (P.char ' ')
  )
 where
  keywords = ["반환하다", "함수", "없음", "참", "거짓", "만약", "이라면", "아니라면", "반복", "인 동안"]
  keyword = P.choice $ reserved <$> keywords
  hangulWord = P.some hangulSyllable
    -- if word `elem` keywords then fail "예약어를 함수 이름으로 쓸 수 없습니다." else return word

parseTerm :: Parser Expr
parseTerm =
  parseNoneExpr
    <|> parseBoolExpr
    <|> parseStringExpr
    <|> P.try parseRealExpr
    <|> parseIntegerExpr
    <|> parseIdentifierExpr
    <|> parseParens
    <|> parseList

parseList :: Parser Expr
parseList = liftA2 List getSourceLine
  $ P.between (symbol "{") (symbol "}") (P.sepBy parseExpr (symbol ","))

parseParens :: Parser Expr
parseParens = P.between (symbol "(") (symbol ")") parseExpr

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = liftA2 Var getSourceLine parseIdentifier

parseIdentifier :: Parser String
parseIdentifier =
  lexeme
      (P.between
        (P.char '[')
        (P.char ']')
        ((++) <$> P.some allowedChars <*> P.many
          (P.char ' ' <|> allowedChars <|> (P.digitChar <?> "숫자"))
        )
      )
    <?> "변수 이름"
 where
  allowedChars = hangulSyllable <|> hangulJamo <|> (P.letterChar <?> "영문")

parseNoneExpr :: Parser Expr
parseNoneExpr = lexeme $ do
  pos <- getSourceLine
  reserved "없음"
  return $ Lit pos LitNone

parseIntegerExpr :: Parser Expr
parseIntegerExpr = lexeme $ do
  pos <- getSourceLine
  val <- zeroNumber <|> parseDecimal
  return $ Lit pos (LitInteger val)
 where
  zeroNumber =
    P.char '0' >> parseHexadecimal <|> parseOctal <|> parseBinary <|> return 0

parseRealExpr :: Parser Expr
parseRealExpr = Lit <$> getSourceLine <*> (LitReal <$> parseReal)

parseStringExpr :: Parser Expr
parseStringExpr = Lit <$> getSourceLine <*> (LitString <$> parseString)

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
parseReal = lexeme L.float

parseString :: Parser String
parseString =
  lexeme
      (P.between (symbol "\'")
                 (symbol "\'")
                 (P.many (P.notFollowedBy (P.char '\'') *> L.charLiteral))
      )
    <?> "문자열"

parseBool :: Parser Bool
parseBool = (True <$ reserved "참") <|> (False <$ reserved "거짓")
