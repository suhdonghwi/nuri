module Main where

import           System.Environment

import           Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runRepl repl (ReplState ">> " "(반응형)")
    filePath : _ -> do
      content <- readFile filePath
      result  <- runRepl (parseInput (toText content))
                         (ReplState ">> " (toText filePath))
      printResult result
