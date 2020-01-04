module Main where

import           System.Environment

import           Repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runRepl repl (ReplState ">> ")
    filePath : _ -> do
      content <- readFile filePath
      result  <- runMaybeT $ parseInput (toText content) (toText filePath)
      whenJust result printResult
