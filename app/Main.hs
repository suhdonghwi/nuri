module Main where

import Prelude hiding (getLine, putStrLn)
import Data.Text.IO (getLine, putStrLn)
import Data.Text as T
import Data.Map as M

import Control.Monad.State.Lazy
import Control.Monad.Except

import Text.Megaparsec

import Nuri.Eval.Expr
import Nuri.Eval.Val
import Nuri.Parse.Expr

runRepl :: SymbolTable -> IO ()
runRepl table = do
  line <- getLine
  let ast = runParser expr "(interactive)" line
  case ast of
    Left err -> (putStrLn . T.pack) $ errorBundlePretty err
    Right result -> do
      let evalResult = runExcept (runStateT (evalExpr result) table)
      case evalResult of
        Left evalErr -> (putStrLn . T.pack) $ show evalErr
        Right finalResult -> (putStrLn . T.pack) $ show finalResult
  runRepl table

main :: IO ()
main = runRepl (M.empty) 