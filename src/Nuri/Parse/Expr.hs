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
    Expr (..),
    UnaryOperator (..),
  )
import Nuri.Parse
  ( Parser,
    reserved,
    sc,
    scn,
  )
import Nuri.Parse.Decl (parseDecl)
import Nuri.Parse.Term (parseNonLexemeTerm, parseTerm)
import Nuri.Parse.Util (parseFuncIdentifier, parseJosa)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

parseExpr :: Parser Expr
parseExpr = parseIf <|> parseSeq <|> parseArithmetic

parseSeq :: Parser Expr
parseSeq = do
  reserved "순서대로" <* P.newline
  scn
  level <- L.indentGuard scn GT P.pos1
  let parseLine = (Left <$> parseDecl parseExpr) <|> (Right <$> parseExpr)
  result <-
    sepBy1
      parseLine
      (P.try $ P.newline >> scn >> L.indentGuard scn EQ level)

  void (P.lookAhead $ P.newline >> L.indentGuard scn LT level) <|> P.eof
  when (isLeft $ last result) $ fail "순서 표현식의 마지막은 선언문이 아닌 표현식이어야 합니다."
  return $ Seq result

parseIf :: Parser Expr
parseIf =
  ( do
      pos <- P.getSourcePos
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
    ((parseNestedFuncCalls <|> parseTerm parseExpr) <?> "표현식")
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
      pos <- P.getSourcePos
      BinaryOp pos op <$ L.symbol sc opStr
    unaryOp opStr op = P.hidden $ do
      pos <- P.getSourcePos
      UnaryOp pos op <$ L.symbol sc opStr

parseNestedFuncCalls :: Parser Expr
parseNestedFuncCalls = do
  calls <- P.try $ P.some (parseFuncCall <?> "함수 호출식")
  processedCalls <- process calls
  return $ foldl1' addArg processedCalls
  where
    process :: [Expr] -> Parser [Expr]
    process (x@(FuncCall _ _ _) : []) = return [x]
    process (FuncCall pos (Var _ ident) args : xs) =
      if T.last ident == '고'
        then do
          let originalIdent = T.snoc (T.init ident) '다'
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
  pos <- P.getSourcePos
  func <- parseFuncIdentifier <?> "함수 이름"
  return $ FuncCall pos (Var pos func) args

parseArguments :: Parser [(Expr, Text)]
parseArguments = P.many $ liftA2 (,) (parseNonLexemeTerm parseExpr <?> "함수 인수") (parseJosa <* sc)
