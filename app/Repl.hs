module Repl where

import           System.Exit
import           System.IO

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Text.IO                  as TextIO
import qualified Data.Text                     as T
import           Data.Map

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Flow
import           Nuri.Parse.Stmt

data ReplState = ReplState { prompt :: T.Text, table :: SymbolTable, fileName :: String }
type Repl = StateT ReplState IO ()

intrinsicTable :: SymbolTable
intrinsicTable = fromList
  [ ( "보여주다"
    , makeFunc
      (initialPos "내장")
      1
      (\[x] -> do
        lift $ lift $ TextIO.putStrLn (printVal x)
        return $ Normal Undefined
      )
    )
  ]

evalInput :: T.Text -> Repl
evalInput input = do
  st <- get
  let ast = runParser (parseStmts <* eof) (fileName st) input
  case ast of
    Left  err    -> lift $ putStrLn (errorBundlePretty err)
    Right result -> do
      evalResult <- lift $ runStmtsEval result (table st)
      case evalResult of
        Left  evalErr       -> lift $ putStrLn (show evalErr)
        Right (_, newTable) -> do
          put $ ReplState (prompt st) newTable (fileName st)

repl :: Repl
repl = do
  st <- get
  lift $ TextIO.putStr (prompt st)
  lift $ hFlush stdout
  line <- T.strip <$> lift (TextIO.getLine)
  lift $ when (line == ":quit") exitSuccess
  evalInput line
  repl

runRepl :: Repl -> ReplState -> IO ()
runRepl f st = runStateT f st >> return ()
