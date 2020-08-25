module Nuri.Parse.Decl where

import qualified Data.Text as T
import Nuri.Expr
  ( Decl (..),
    DeclKind (..),
    DeclType (ConstDecl, FuncDecl),
    Expr,
  )
import Nuri.Parse
  ( Parser,
    lexeme,
    reserved,
    sc,
    scn,
    symbol,
  )
import Nuri.Parse.Term (parseIdentifier)
import Nuri.Parse.Util (parseFuncIdentifier, parseJosa)
import qualified Text.Megaparsec as P

parseDecl :: Parser Expr -> Parser Decl
parseDecl e = parseConstDecl e <|> parseFuncDecl e

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

parseFuncDecl :: Parser Expr -> Parser Decl
parseFuncDecl parseExpr = do
  pos <- P.getSourcePos
  declKind <- parseDeclKind
  args <- parseArgList []
  offset <- P.getOffset
  funcName <- parseFuncIdentifier
  checkValidIdentifier offset declKind funcName

  colon <- P.observing (symbol ":")
  case colon of
    Left _ -> return $ Decl pos funcName Nothing
    Right _ -> do
      scn
      result <- Decl pos funcName <$> (Just . FuncDecl declKind args <$> parseExpr)
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

parseConstDecl :: Parser Expr -> Parser Decl
parseConstDecl parseExpr = do
  pos <- P.getSourcePos
  reserved "상수"
  identifier <- lexeme parseIdentifier <* symbol ":"
  Decl pos identifier <$> Just . ConstDecl <$> parseExpr
