module Main where

import           System.Environment

import qualified Data.Text                     as T

import           Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runRepl repl (ReplState ">> " intrinsicTable "(반응형)")
    filePath : _ -> do
      content <- readFile filePath
      runRepl (evalInput (T.pack content))
              (ReplState ">> " intrinsicTable filePath)
