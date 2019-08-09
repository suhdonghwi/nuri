module Main where

import           System.Environment

import qualified Data.Text                     as T

import           Repl

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl repl (ReplState ">> " intrinsicTable "(반응형)")
    else do
      let filePath = head args
      content <- readFile filePath
      runRepl (evalInput (T.pack content))
              (ReplState ">> " intrinsicTable filePath)
