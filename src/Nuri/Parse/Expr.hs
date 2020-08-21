module Nuri.Parse.Expr where

import Control.Monad.Combinators.Expr
  ( Operator
      ( InfixL,
        Prefix
      ),
    makeExprParser,
  )
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List (foldl1')
import qualified Data.Text as T
import Nuri.Expr
  ( BinaryOperator (..),
    Decl (..),
    DeclKind (..),
    DeclType (ConstDecl, FuncDecl),
    Expr (..),
    UnaryOperator (..),
  )
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
    resolveDecl,
    sc,
    scn,
    symbol,
  )
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding
  ( fromList,
    unwords,
  )

parseKeyword :: Parser ()
parseKeyword = P.choice $ reserved <$> keywords
  where
    keywords =
      [ "함수",
        "동사",
        "형용사",
        "없음",
        "참",
        "거짓",
        "만약",
        "이라면",
        "아니라면",
        "순서대로",
        "그리고",
        "또는"
      ]

parseDecl :: Parser Decl
parseDecl = parseConstDecl <|> parseFuncDecl

parseDeclKind :: Parser DeclKind
parseDeclKind =
  (pure NormalDecl <* reserved "함수")
    <|> (pure VerbDecl <* reserved "동사")
    <|> (pure AdjectiveDecl <* reserved "형용사")

checkValidIdentifier :: Int -> DeclKind -> Text -> Parser ()
checkValidIdentifier offset kind name = do
  if kind `elem` [VerbDecl, AdjectiveDecl]
    then when (not $ T.last name == '다') $ do
      P.setOffset offset
      fail "용언을 선언할 때는 식별자가 ~(하)다 꼴이어야 합니다."
    else pass

parseFuncDecl :: Parser Decl
parseFuncDecl = do
  pos <- getSourceLine
  declKind <- parseDeclKind
  args <- parseArgList []
  offset <- P.getOffset
  funcName <- parseFuncIdentifier
  checkValidIdentifier offset declKind funcName

  modify ((declKind, funcName) :)

  colon <- P.observing (symbol ":")
  case colon of
    Left _ -> return $ Decl pos declKind funcName Nothing
    Right _ -> do
      scn
      st <- get
      modify (++ ((NormalDecl,) <$> (fst <$> args)))
      result <- Decl pos declKind funcName <$> (Just . FuncDecl args <$> parseExpr)
      put st
      return result
  where
    parseArgList :: [(Text, Text)] -> Parser [(Text, Text)]
    parseArgList l = do
      identPos <- P.getOffset
      identResult <- P.observing parseIdentifier
      case identResult of
        Left _ -> return l
        Right ident -> do
          josaPos <- P.getOffset
          josa <- parseJosa
          sc
          when
            (ident `elem` (fst <$> l))
            ( do
                P.setOffset (identPos + 1)
                fail "함수 인자의 이름이 중복됩니다."
            )
          when
            (josa `elem` (snd <$> l))
            ( do
                P.setOffset josaPos
                fail "조사는 중복되게 사용할 수 없습니다."
            )
          parseArgList (l ++ [(ident, josa)])

parseJosa :: Parser Text
parseJosa =
  ( do
      P.notFollowedBy parseKeyword
      josa <- toText <$> P.some hangulSyllable
      return
        ( case josa of
            "으로" -> "로"
            "과" -> "와"
            "를" -> "을"
            "는" -> "은"
            "가" -> "이"
            j -> j
        )
  )
    <?> "조사"

parseConstDecl :: Parser Decl
parseConstDecl = do
  pos <- getSourceLine
  reserved "상수"
  identifier <- lexeme parseIdentifier <* symbol ":"
  modify ((NormalDecl, identifier) :)
  Decl pos NormalDecl identifier <$> Just . ConstDecl <$> parseExpr

parseExpr :: Parser Expr
parseExpr = parseIf <|> parseSeq <|> parseArithmetic

parseSeq :: Parser Expr
parseSeq = do
  reserved "순서대로" <* P.newline
  scn
  level <- L.indentGuard scn GT P.pos1
  let parseLine = (Left <$> parseDecl) <|> (Right <$> parseExpr)
  st <- get
  result <-
    sepBy1
      parseLine
      (P.try $ P.newline >> scn >> L.indentGuard scn EQ level)
  when (isLeft $ last result) $ fail "순서 표현식의 마지막은 선언문이 아닌 표현식이어야 합니다."
  put st
  return $ Seq result

parseIf :: Parser Expr
parseIf =
  ( do
      pos <- getSourceLine
      reserved "만약"
      condExpr <- parseExpr
      scn
      reserved "이라면"
      scn
      thenExpr <- parseExpr
      scn
      reserved "아니라면"
      scn
      If pos condExpr thenExpr <$> parseExpr
  )
    <?> "조건식"

parseArithmetic :: Parser Expr
parseArithmetic =
  makeExprParser
    ((P.try parseNestedFuncCalls <|> parseTerm) <?> "표현식")
    table
  where
    table =
      [ [ Prefix $ unaryOp "+" Positive,
          Prefix $ unaryOp "-" Negative,
          Prefix $ unaryOp "!" LogicNot
        ],
        [ InfixL $ binaryOp "*" Multiply,
          InfixL $ binaryOp "/" Divide,
          InfixL $ binaryOp "%" Mod
        ],
        [InfixL $ binaryOp "+" Add, InfixL $ binaryOp "-" Subtract],
        [InfixL $ binaryOp "==" Equal, InfixL $ binaryOp "!=" Inequal],
        [ InfixL $ binaryOp "<=" LessThanEqual,
          InfixL $ binaryOp ">=" GreaterThanEqual,
          InfixL $ binaryOp "<" LessThan,
          InfixL $ binaryOp ">" GreaterThan
        ],
        [ InfixL $ binaryOp "그리고" LogicAnd,
          InfixL $ binaryOp "또는" LogicOr
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
  calls <- P.some (parseFuncCall <?> "함수 호출식")
  processedCalls <- process calls
  return $ foldl1' addArg processedCalls
  where
    process :: [Expr] -> Parser [Expr]
    process (x@(FuncCall _ (Var _ ident) _) : []) = do
      _ <- resolveDecl ident [NormalDecl, VerbDecl, AdjectiveDecl] 0
      return [x]
    process (FuncCall pos (Var _ ident) args : xs) =
      if T.last ident == '고'
        then do
          let originalIdent = T.snoc (T.init ident) '다'
          _ <- resolveDecl originalIdent [VerbDecl] 0
          pxs <- process xs
          return (FuncCall pos (Var pos originalIdent) args : pxs)
        else do
          -- offset 설정
          fail "여기에서는 활용이 '~하고' 형태여야합니다."
    process _ = error "불가능한 상황"

    addArg :: Expr -> Expr -> Expr
    addArg arg (FuncCall pos func args) =
      FuncCall pos func ((arg, "_") : args)
    addArg _ _ = error "불가능한 상황"

parseFuncCall :: Parser Expr
parseFuncCall = do
  args <- parseArguments
  pos <- getSourceLine
  func <- parseFuncIdentifier <?> "함수 이름"
  return $ FuncCall pos (Var pos func) args

parseArguments :: Parser [(Expr, Text)]
parseArguments = P.many $ liftA2 (,) (parseNonLexemeTerm <?> "함수 인수") (parseJosa <* sc)

parseFuncIdentifier :: Parser Text
parseFuncIdentifier = lexeme (P.notFollowedBy parseKeyword *> hangulWord)
  where
    hangulWord = toText <$> P.some (hangulSyllable <|> P.char '_')

parseTerm :: Parser Expr
parseTerm =
  lexeme
    ( parseNoneExpr
        <|> parseBoolExpr
        <|> parseCharExpr
        <|> P.try (parseRealExpr)
        <|> parseIntegerExpr
        <|> parseIdentifierExpr
        <|> parseParens
    )

parseNonLexemeTerm :: Parser Expr
parseNonLexemeTerm =
  parseNoneExpr
    <|> parseBoolExpr
    <|> parseCharExpr
    <|> P.try (parseRealExpr)
    <|> parseIntegerExpr
    <|> parseIdentifierExpr
    <|> parseParens

parseParens :: Parser Expr
parseParens = P.between (P.char '(' >> sc) (sc >> P.char ')') parseExpr

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = do
  pos <- getSourceLine
  offset <- P.getOffset
  ident <- parseIdentifier
  _ <- resolveDecl ident [NormalDecl, VerbDecl, AdjectiveDecl] offset
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
