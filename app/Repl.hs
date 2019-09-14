module Repl where

import           System.IO                                ( hFlush )

import           Data.Text                                ( strip )

import           Control.Lens
import           Control.Lens.TH                          ( )

import           Data.Text.Prettyprint.Doc                ( pretty )

import           Text.Megaparsec

import           Nuri.Stmt
import           Nuri.Pretty                              ( )
import           Nuri.Parse.Stmt

data ReplState = ReplState { _prompt :: Text, _fileName :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: Text -> Repl (Maybe Stmt)
parseInput input = do
  st <- get
  case runParser (parseStmt <* eof) (toString $ view fileName st) input of
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
    Just val -> (liftIO . print) (pretty val)
    Nothing  -> pass

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
