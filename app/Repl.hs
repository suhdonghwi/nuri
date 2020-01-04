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
import           Haneul.Pretty                            ( )

data ReplState = ReplState { _prompt :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: Text -> Text -> MaybeT IO Stmts
parseInput input fileName = do
  case runParser (parseStmts <* eof) (toString fileName) input of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      hoistMaybe Nothing
    Right parseResult -> return parseResult

printResult :: Stmts -> IO ()
printResult val = do
  (liftIO . print . vsep . toList) (pretty <$> val)
  let (internal, insts) =
        execRWS (sequence $ compileStmt <$> val) "(반응형)" defaultInternal
  putStrLn "---------------"
  print $ pretty internal
  (print . vsep) (pretty <$> insts)

repl :: Repl ()
repl = forever $ do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  input <- strip <$> liftIO getLine
  liftIO $ when (input == ":quit") exitSuccess
  result <- (liftIO . runMaybeT . parseInput input) "(반응형)"
  case result of
    Just stmts -> liftIO (printResult stmts)
    Nothing    -> pass

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
