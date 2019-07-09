module Main where

import           Prelude

import           System.Exit
import           System.Environment
import           System.IO                     as IO

import qualified Data.Text.IO                  as TextIO
import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Control.Monad.State.Lazy
import           Control.Monad.Except

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import           Nuri.Eval.Val
import           Nuri.Eval.Flow
import           Nuri.Parse.Stmt

evalInput :: T.Text -> Map.Map T.Text Val -> String -> IO ()
evalInput input table fileName = do
  let ast = runParser (stmts <* eof) fileName input
  case ast of
    Left  err    -> IO.putStrLn $ errorBundlePretty err
    Right result -> do
      let evalResult =
            runExcept (runStateT (runFlowT (evalStmts result False)) table)
      case evalResult of
        Left  evalErr     -> IO.putStrLn $ show evalErr
        Right finalResult -> IO.putStrLn $ show finalResult

runRepl :: T.Text -> SymbolTable -> IO ()
runRepl prompt table = do
  TextIO.putStr prompt
  hFlush stdout
  line <- T.strip <$> TextIO.getLine
  when (line == ":quit") exitSuccess
  evalInput line table "(반응형)"
  runRepl prompt table

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then runRepl ">> " Map.empty
    else do
      let fileName = head args
      content <- readFile fileName
      evalInput (T.pack content) Map.empty fileName
