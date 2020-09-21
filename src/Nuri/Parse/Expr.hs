module Nuri.Parse.Expr where

import Control.Monad.Combinators.Expr
  ( Operator
      ( InfixL,
        Prefix
      ),
    makeExprParser,
  )
import Control.Monad.Combinators.NonEmpty (sepBy1, some)
import Data.List (foldl1')
import qualified Data.Text as T
import Nuri.Expr
  ( BinaryOperator (..),
    Decl (Decl),
    DeclKind (..),
    DeclType (StructDecl),
    Expr (..),
    UnaryOperator (..),
  )
import Nuri.Parse
  ( MonadParser,
    reserved,
    sc,
    scn,
  )
import Nuri.Parse.Decl (parseDecl)
import Nuri.Parse.PartTable (checkDeclKind)
import Nuri.Parse.Term (parseNonLexemeTerm, parseTerm)
import Nuri.Parse.Util (parseFuncIdentifier, parseJosa)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

parseExpr :: (MonadParser m) => m Expr
parseExpr = parseIf <|> parseSeq <|> parseArithmetic

parseSeq :: (MonadParser m) => m Expr
parseSeq = do
  reserved "순서대로" <* P.eol
  scn
  level <- L.indentGuard scn GT P.pos1

  st <- get
  result <-
    sepBy1
      parseLine
      (P.try $ P.eol >> scn >> L.indentGuard scn EQ level)
  put st

  void (P.lookAhead $ P.eol >> L.indentGuard scn LT level) <|> P.eof
  when (isLeft $ last result) $ fail "순서 표현식의 마지막은 선언문이 아닌 표현식이어야 합니다."
  return $ Seq result
  where
    parseDeclExceptStruct :: (MonadParser m) => m Decl
    parseDeclExceptStruct = do
      offset <- P.getOffset
      decl@(Decl _ _ declType) <- parseDecl parseExpr
      case declType of
        StructDecl _ -> do
          P.setOffset offset
          fail "순서 표현식에는 구조체 선언문이 올 수 없습니다."
        _ -> pass
      return decl

    parseLine = (Left <$> parseDeclExceptStruct) <|> (Right <$> parseExpr)

parseIf :: (MonadParser m) => m Expr
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

parseArithmetic :: (MonadParser m) => m Expr
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

parseNestedFuncCalls :: (MonadParser m) => m Expr
parseNestedFuncCalls = do
  calls <- P.try $ some (parseFuncCall <?> "함수 호출식")
  case calls of
    (x, _) :| [] -> return x
    _ -> do
      processedCalls <- process calls
      return $ foldl1' addArg processedCalls
  where
    process :: (MonadParser m) => NonEmpty (Expr, Int) -> m [Expr]
    process ((x@(FuncCall pos (Var _ ident) args), offset) :| xs') =
      case nonEmpty xs' of
        Nothing -> do
          checkDeclKind offset ident VerbDecl
          return [x]
        Just xs ->
          if T.last ident == '고'
            then do
              let originalIdent = T.snoc (T.init ident) '다'
              checkDeclKind offset originalIdent VerbDecl
              pxs <- process xs
              return (FuncCall pos (Var pos originalIdent) args : pxs)
            else do
              P.setOffset offset
              fail "여기에서는 활용이 '~(하)고' 형태여야합니다."
    process _ = error "불가능한 상황"

    addArg :: Expr -> Expr -> Expr
    addArg arg (FuncCall pos func args) =
      FuncCall pos func ((arg, "_") : args)
    addArg _ _ = error "불가능한 상황"

parseFuncCall :: (MonadParser m) => m (Expr, Int)
parseFuncCall = do
  args <- parseArguments
  pos <- P.getSourcePos
  offset <- P.getOffset
  func <- parseFuncIdentifier <?> "함수 이름"
  return (FuncCall pos (Var pos func) args, offset)

parseArguments :: (MonadParser m) => m [(Expr, Text)]
parseArguments = P.many $ liftA2 (,) (parseNonLexemeTerm parseExpr <?> "함수 인수") (parseJosa <* sc)
