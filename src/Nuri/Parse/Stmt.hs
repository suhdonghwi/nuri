module Nuri.Parse.Stmt where

import           Control.Monad

import           Data.List.NonEmpty
import           Data.Maybe

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

parseStmts :: Parser [Stmt]
parseStmts = many (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt =
  try parseIfStmt
    <|> try parseReturnStmt
    <|> try parseExprStmt
    <|> parseFuncDecl

parseExprStmt :: Parser Stmt
parseExprStmt =
  ExprStmt
    <$> (parseExpr <* notFollowedBy (reserved "반환하다" <|> void (symbol ":")))

parseReturnStmt :: Parser Stmt
parseReturnStmt = Return <$> (parseExpr <* reserved "반환하다")

parseIfStmt :: Parser Stmt
parseIfStmt = do
  pos      <- getSourcePos
  ifPart   <- L.indentBlock scn (ifLine "만약")
  elifPart <- optional (many $ L.indentBlock scn (ifLine "아니고"))
  elsePart <- optional (L.indentBlock scn (elseLine "아니면"))
  return $ If pos (ifPart :| fromMaybe [] elifPart) elsePart
 where
  ifLine s = do
    _ <- reserved s
    e <- parseExpr
    _ <- (reserved "면" <|> reserved "이면" <|> reserved "이라면")
    _ <- symbol ":"
    return (L.IndentSome Nothing (return . (,) e) parseStmt)
  elseLine s = do
    _ <- reserved s
    _ <- symbol ":"
    return (L.IndentSome Nothing return parseStmt)

parseFuncDecl :: Parser Stmt
parseFuncDecl = L.indentBlock scn argsLine
 where
  argsLine = do
    pos  <- getSourcePos
    args <- many (char '[' *> parseIdentifier <* (char ']' >> sc))
    sc
    funcName <- parseFuncIdentifier
    _        <- symbol ":"
    return
      (L.IndentSome Nothing (return . FuncDecl pos funcName args) parseStmt)
