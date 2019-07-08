module Main where

import Prelude

import System.Exit
import System.IO as IO

import Data.Text.IO as TextIO
import Data.Text as Text
import Data.Map as Map

import Control.Monad.State.Lazy
import Control.Monad.Except

import Text.Megaparsec

import Nuri.Eval.Expr
import Nuri.Eval.Stmt
import Nuri.Eval.Val
import Nuri.Parse.Expr

runRepl :: Text -> SymbolTable -> IO ()
runRepl prompt table = do
  TextIO.putStr prompt
  hFlush stdout
  line <- TextIO.getLine
  when (line == ":quit") exitSuccess
  let ast = runParser expr "(interactive)" line
  case ast of
    Left err -> IO.putStrLn $ errorBundlePretty err
    Right result -> do
      let evalResult = runExcept (runStateT (evalExpr result) table)
      case evalResult of
        Left evalErr -> IO.putStrLn $ show evalErr
        Right finalResult -> IO.putStrLn $ show finalResult
  runRepl prompt table

main :: IO ()
main = runRepl ">> " Map.empty 