{-# LANGUAGE ScopedTypeVariables #-}
module Nuri.Parse.Stmt where

import Control.Monad.Combinators.NonEmpty (some)
import Nuri.Parse (MonadParser, reserved, sc, scn)
import Nuri.Parse.Decl (parseDecl)
import Nuri.Parse.Error (errorBundlePretty)
import Nuri.Parse.Expr (parseExpr)
import Nuri.Stmt (Stmt (..))
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

parseStmts :: (MonadParser m, MonadIO m) => m (NonEmpty Stmt)
parseStmts = scn >> join <$> some (L.nonIndented sc parseStmt <* scn)

parseStmt :: (MonadParser m, MonadIO m) => m (NonEmpty Stmt)
parseStmt = (singleton <$> parseDeclStmt) <|> (singleton <$> parseExprStmt) <|> parseImportStmt
  where
    singleton x = x :| []

parseDeclStmt :: (MonadParser m) => m Stmt
parseDeclStmt = DeclStmt <$> parseDecl parseExpr

parseExprStmt :: (MonadParser m) => m Stmt
parseExprStmt = ExprStmt <$> parseExpr

parseImportStmt :: (MonadParser m, MonadIO m) => m (NonEmpty Stmt)
parseImportStmt = do
  reserved "꾸러미"
  path <- P.between (P.char '"') (P.char '"') (P.many (P.notFollowedBy (P.char '"') >> L.charLiteral))
  currentPath <- P.sourceName <$> P.getSourcePos
  let realPath = takeDirectory currentPath </> path

  exists <- liftIO $ doesFileExist realPath
  when (not exists) $ do
    fail $ "'" ++ realPath ++ "' 파일을 찾을 수 없습니다."

  content <- readFileText realPath
  st <- P.getParserState
  result <- liftIO $ parseInput content realPath
  P.setParserState st

  return result

parseInput :: Text -> String -> IO (NonEmpty Stmt)
parseInput input fileName = do
  (r, _) <- runStateT (P.runParserT (parseStmts <* P.eof) fileName input) (fromList [])
  case r of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      exitSuccess
    Right result -> return result
