module Nuri.Parse.Stmt where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char.Lexer    as L

import           Nuri.Parse
import           Nuri.Parse.Expr
import           Nuri.Stmt

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
  ifPart   <- L.indentBlock scn (ifLine "만약")
  elifPart <- P.many $ L.indentBlock scn (ifLine "아니고")
  elsePart <- optional $ L.indentBlock scn (elseLine "아니면")
  return $ ifPart (foldr (??) elsePart (Just <$> elifPart))
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
parseFuncDecl = L.indentBlock scn argsLine
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
