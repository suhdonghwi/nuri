module Repl where

import           System.IO                                ( hFlush )

import           Control.Lens
import           Control.Lens.TH                          ( )

import           Data.Text                                ( strip )

import           Text.Megaparsec

import           Nuri.Stmt
import           Nuri.Parse.Stmt

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

repl :: Repl ()
repl = forever $ do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  input <- strip <$> liftIO getLine
  liftIO $ when (input == ":quit") exitSuccess
  result <- parseInput input
  case result of
    Just val -> liftIO $ print val
    Nothing  -> pass

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
