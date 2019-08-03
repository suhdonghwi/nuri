module Repl where

import           System.Exit
import           System.IO

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans

import qualified Data.Text.IO                  as TextIO
import qualified Data.Text                     as T
import           Data.Map

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Parse.Stmt

data ReplState = ReplState { prompt :: T.Text, table :: SymbolTable, fileName :: String }
type Repl = StateT ReplState IO

intrinsicTable :: SymbolTable
intrinsicTable = fromList
  [ ( "보여주다"
    , makeFunc
      (initialPos "내장")
      1
      (\[x] -> do
        liftIO $ TextIO.putStrLn (printVal x)
        return (Normal Undefined)
      )
    )
  ]

evalInput :: T.Text -> Repl (Maybe (Flow Val, SymbolTable))
evalInput input = do
  st <- get
  let ast = runParser (parseStmts <* eof) (fileName st) input
  case ast of
    Left  err    -> lift $ putStrLn (errorBundlePretty err) >> return Nothing
    Right result -> do
      evalResult <- lift $ runStmtsEval result (table st)
      case evalResult of
        Left evalErr -> lift $ putStrLn (show evalErr) >> return Nothing
        Right (finalValue, newTable) -> do
          put $ ReplState (prompt st) newTable (fileName st)
          return $ Just (finalValue, newTable)

repl :: Repl ()
repl = do
  st <- get
  lift $ TextIO.putStr (prompt st)
  lift $ hFlush stdout
  line <- T.strip <$> lift (TextIO.getLine)
  lift $ when (line == ":quit") exitSuccess
  result <- evalInput line
  case result of
    Just (val, _) -> lift $ print val
    Nothing       -> return ()
  repl

runRepl :: Repl a -> ReplState -> IO ()
runRepl f st = runStateT f st >> return ()
