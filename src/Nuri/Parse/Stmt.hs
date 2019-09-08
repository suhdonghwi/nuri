module Nuri.Parse.Stmt where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

parseIndent :: Parser (L.IndentOpt Parser a b) -> Parser a
parseIndent p = do
  oldState <- get
  r        <- L.indentBlock scn p
  put oldState
  return r

parseStmts :: Parser Stmt
parseStmts = Seq . fromList <$> P.some (parseStmt <* scn)

parseStmt :: Parser Stmt
parseStmt =
  parseIfStmt <|> P.try parseReturnStmt <|> parseFuncDecl <|> parseExprStmt

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> parseExpr

parseReturnStmt :: Parser Stmt
parseReturnStmt = Return <$> (parseExpr <* reserved "반환하다")

parseIfStmt :: Parser Stmt
parseIfStmt = do
  ifPart   <- parseIndent (ifLine "만약")
  elifPart <- P.many $ parseIndent (ifLine "아니고")
  elsePart <- optional $ parseIndent (elseLine "아니면")
  return $ ifPart (foldr ($) elsePart (fmap Just <$> elifPart))
 where
  ifLine s = do
    pos <- P.getSourcePos
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
parseFuncDecl = do
  stmt@(FuncDecl _ funcName _ _) <- parseIndent argsLine
  modify (funcName :)
  return stmt
 where
  argsLine = P.try $ do
    pos  <- P.getSourcePos
    args <- P.many parseIdentifier
    sc
    funcName <- parseFuncIdentifier
    _        <- symbol ":"
    return
      (L.IndentSome Nothing
                    (return . FuncDecl pos funcName args . Seq . fromList)
                    parseStmt
      )
