module Main where

import           System.Environment

import           Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runRepl repl (ReplState ">> ")
    filePath : _ -> do
      content <- readFileText filePath
      result  <- runMaybeT $ parseInput content filePath
      whenJust result printResult
