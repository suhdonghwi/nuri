module Repl where

import           Prelude

import           System.Exit
import           System.IO

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Text.IO                  as TextIO
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Flow
import           Nuri.Parse.Stmt

evalInput :: T.Text -> Map.Map T.Text Val -> String -> IO ()
evalInput input table fileName = do
  let ast = runParser (stmts <* eof) fileName input
  case ast of
    Left  err    -> putStrLn $ errorBundlePretty err
    Right result -> do
      let evalResult =
            runExcept (runStateT (runFlowT (evalStmts result False)) table)
      case evalResult of
        Left  evalErr     -> putStrLn $ show evalErr
        Right finalResult -> putStrLn $ show finalResult

runRepl :: T.Text -> SymbolTable -> IO ()
runRepl prompt table = do
  TextIO.putStr prompt
  hFlush stdout
  line <- T.strip <$> TextIO.getLine
  when (line == ":quit") exitSuccess
  evalInput line table "(반응형)"
  runRepl prompt table
