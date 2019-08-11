module Nuri.Parse.Stmt where

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

parseStmts :: Parser [Stmt]
parseStmts = many (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt =
  parseIfStmt <|> try parseReturnStmt <|> try parseFuncDecl <|> parseExprStmt

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> parseExpr

parseReturnStmt :: Parser Stmt
parseReturnStmt = Return <$> (parseExpr <* reserved "반환하다")

parseIfStmt :: Parser Stmt
parseIfStmt = do
  ifPart   <- L.indentBlock scn (ifLine "만약")
  elifPart <- many $ L.indentBlock scn (ifLine "아니고")
  elsePart <- optional $ L.indentBlock scn (elseLine "아니면")
  return $ ifPart (foldr (??) elsePart (Just <$> elifPart))
 where
  f ?? a = f <*> return a
  ifLine s = do
    pos <- getSourcePos
    _   <- reserved s
    e   <- parseExpr
    _   <- reserved "면" <|> reserved "이면" <|> reserved "이라면"
    _   <- symbol ":"
    return (L.IndentSome Nothing (return . If pos e . Seq . fromList) parseStmt)
  elseLine s = do
    _ <- reserved s
    _ <- symbol ":"
    return (L.IndentSome Nothing (return . Seq . fromList) parseStmt)

parseFuncDecl :: Parser Stmt
parseFuncDecl = L.indentBlock scn argsLine
 where
  argsLine = do
    pos  <- getSourcePos
    args <- many parseIdentifier
    sc
    funcName <- parseFuncIdentifier
    _        <- symbol ":"
    return
      (L.IndentSome Nothing
                    (return . FuncDecl pos funcName args . Seq . fromList)
                    parseStmt
      )
