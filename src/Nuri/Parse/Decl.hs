module Nuri.Parse.Decl where

import qualified Data.Text as T
import Nuri.Expr
  ( Decl (..),
    DeclType (..),
    Expr,
    FuncKind (..),
    FuncVariation (Synonym, Antonym)
  )
import Nuri.Parse
  ( MonadParser,
    lexeme,
    reserved,
    sc,
    scn,
    symbol,
  )
import Nuri.Parse.PartTable (MonadPartTable, addDecl)
import Nuri.Parse.Term (parseIdentifier)
import Nuri.Parse.Util (parseFuncIdentifier, parseJosa, parseStructIdentifier)
import qualified Text.Megaparsec as P

parseDecl :: (MonadParser m) => m Expr -> m Decl
parseDecl e = parseConstDecl e <|> parseFuncDecl e <|> parseStructDecl

parseDeclKind :: (MonadParser m) => m FuncKind
parseDeclKind =
  (pure Normal <* reserved "함수")
    <|> (pure Verb <* reserved "동사")
    <|> (pure Adjective <* reserved "형용사")

checkValidIdentifier :: (MonadParser m) => Int -> FuncKind -> Text -> m ()
checkValidIdentifier offset kind name = do
  if kind `elem` [Verb, Adjective]
    then when (not $ T.last name == '다') $ do
      P.setOffset offset
      fail "용언을 선언할 때는 식별자가 ~(하)다 꼴이어야 합니다."
    else pass

parseFuncDecl :: (MonadParser m) => m Expr -> m Decl
parseFuncDecl parseExpr = do
  pos <- P.getSourcePos
  declKind <- parseDeclKind
  args <- parseArgList []
  offset <- P.getOffset
  funcName <- parseFuncIdentifier

  synAnts <- if declKind == Adjective then parseSynAnt <|> pure [] else pure []

  let addVariation (Synonym t) = addDecl t Adjective
      addVariation (Antonym t) = addDecl t Adjective

  checkValidIdentifier offset declKind funcName
  addDecl funcName declKind
  sequence_ $ addVariation <$> synAnts

  colon <- P.observing (symbol ":")

  let funcDecl body = Decl pos funcName $ case declKind of
        Normal -> FuncDecl args body
        Verb -> VerbDecl args body
        Adjective -> AdjectiveDecl args synAnts body

  case colon of
    Left _ -> return $ funcDecl Nothing
    Right _ -> do
      scn
      body <- Just <$> parseExpr
      return $ funcDecl body
  where
    parseSynonym :: (MonadParser m) => m FuncVariation
    parseSynonym = symbol "=" >> Synonym <$> parseAdjectiveIdentifier

    parseAntonym :: (MonadParser m) => m FuncVariation
    parseAntonym = symbol "<->" >> Antonym <$> parseAdjectiveIdentifier

    parseAdjectiveIdentifier :: (MonadParser m) => m Text
    parseAdjectiveIdentifier = do
      offset <- P.getOffset
      funcName <- parseFuncIdentifier
      checkValidIdentifier offset Adjective funcName
      return funcName

    parseSynAnt :: (MonadParser m) => m [FuncVariation]
    parseSynAnt = 
      P.between (symbol "(") (symbol ")") ((parseSynonym <|> parseAntonym) `P.sepBy1` symbol ",")

    parseArgList :: (MonadParser m) => [(Text, Text)] -> m [(Text, Text)]
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

parseConstDecl :: (MonadParser m) => m Expr -> m Decl
parseConstDecl parseExpr = do
  pos <- P.getSourcePos
  reserved "상수"
  identifier <- lexeme parseIdentifier <* symbol ":"
  Decl pos identifier <$> ConstDecl <$> parseExpr

parseStructDecl :: (MonadParser m, MonadPartTable m) => m Decl
parseStructDecl = do
  pos <- P.getSourcePos
  reserved "구조체"
  identifier <- lexeme parseStructIdentifier <* symbol ":"
  fields <- parseFuncIdentifier `P.sepBy1` symbol ","

  sequence_ $ (`addDecl` Normal) <$> fields
  return $ Decl pos identifier (StructDecl fields)
