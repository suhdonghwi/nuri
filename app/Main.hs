module Main where

import           System.Environment

import           Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runRepl repl (ReplState ">> " intrinsicTable "(반응형)")
    filePath : _ -> do
      content <- readFile filePath
      runRepl (evalInput (toText content))
              (ReplState ">> " intrinsicTable (toText filePath))
