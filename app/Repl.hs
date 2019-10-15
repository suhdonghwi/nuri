module Repl where

import           System.IO                                ( hFlush )

import           Data.Text                                ( strip )

import           Control.Monad.RWS                        ( execRWS )
import           Control.Lens
import           Control.Lens.TH                          ( )

import           Data.Text.Prettyprint.Doc                ( pretty
                                                          , vsep
                                                          )

import           Text.Megaparsec

import           Nuri.Stmt
import           Nuri.Pretty                              ( )
import           Nuri.Parse.Stmt
import           Nuri.Codegen.Stmt

import           Haneul.Builder

data ReplState = ReplState { _prompt :: Text, _fileName :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: Text -> Repl (Maybe Stmts)
parseInput input = do
  st <- get
  case runParser (parseStmts <* eof) (toString $ view fileName st) input of
    Left err -> do
      liftIO $ (putTextLn . toText . errorBundlePretty) err
      return Nothing
    Right parseResult -> do
      return (Just parseResult)

printResult :: Maybe Stmts -> IO ()
printResult result = case result of
  Just val -> do
    (liftIO . print . vsep . toList) (pretty <$> val)
    let code = execRWS (sequence $ compileStmt <$> val) () defaultInternal
    putStrLn "---------------"
    print code
  Nothing -> pass

repl :: Repl ()
repl = forever $ do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  input <- strip <$> liftIO getLine
  liftIO $ when (input == ":quit") exitSuccess
  result <- parseInput input
  liftIO $ printResult result

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
